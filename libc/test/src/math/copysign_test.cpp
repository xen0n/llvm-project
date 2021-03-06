//===-- Unittests for copysign --------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "src/math/copysign.h"
#include "utils/FPUtil/FPBits.h"
#include "utils/FPUtil/TestHelpers.h"
#include "utils/UnitTest/Test.h"
#include <math.h>

using FPBits = __llvm_libc::fputil::FPBits<double>;

DECLARE_SPECIAL_CONSTANTS(double)

TEST(LlvmLibcCopySignTest, SpecialNumbers) {
  EXPECT_FP_EQ(aNaN, __llvm_libc::copysign(aNaN, -1.0));
  EXPECT_FP_EQ(aNaN, __llvm_libc::copysign(aNaN, 1.0));

  EXPECT_FP_EQ(negInf, __llvm_libc::copysign(inf, -1.0));
  EXPECT_FP_EQ(inf, __llvm_libc::copysign(negInf, 1.0));

  EXPECT_FP_EQ(negZero, __llvm_libc::copysign(zero, -1.0));
  EXPECT_FP_EQ(zero, __llvm_libc::copysign(negZero, 1.0));
}

TEST(LlvmLibcCopySignTest, InDoubleRange) {
  using UIntType = FPBits::UIntType;
  constexpr UIntType count = 10000000;
  constexpr UIntType step = UIntType(-1) / count;
  for (UIntType i = 0, v = 0; i <= count; ++i, v += step) {
    double x = FPBits(v);
    if (isnan(x) || isinf(x) || x == 0)
      continue;

    double res1 = __llvm_libc::copysign(x, -x);
    ASSERT_FP_EQ(res1, -x);

    double res2 = __llvm_libc::copysign(x, x);
    ASSERT_FP_EQ(res2, x);
  }
}
