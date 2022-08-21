//===--- loongarch.cpp - Generic JITLink loongarch edge kinds, utilities --===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Generic utilities for graphs representing loongarch objects.
//
//===----------------------------------------------------------------------===//

#include "llvm/ExecutionEngine/JITLink/loongarch.h"

#define DEBUG_TYPE "jitlink"

namespace llvm {
namespace jitlink {
namespace loongarch {

const char *getEdgeKindName(Edge::Kind K) {
  switch (K) {
  case R_LARCH_32:
    return "R_LARCH_32";
  case R_LARCH_64:
    return "R_LARCH_64";
  }
  return getGenericEdgeKindName(K);
}
} // namespace loongarch
} // namespace jitlink
} // namespace llvm
