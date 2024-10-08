; NOTE: Assertions have been autogenerated by utils/update_test_checks.py UTC_ARGS: --version 4
; RUN: opt < %s -passes=instcombine -S | FileCheck %s

define i64 @sext_xor_sub(i64 %a, i1 %b) {
; CHECK-LABEL: define i64 @sext_xor_sub(
; CHECK-SAME: i64 [[A:%.*]], i1 [[B:%.*]]) {
; CHECK-NEXT:    [[TMP1:%.*]] = sub i64 0, [[A]]
; CHECK-NEXT:    [[R:%.*]] = select i1 [[B]], i64 [[TMP1]], i64 [[A]]
; CHECK-NEXT:    ret i64 [[R]]
;
  %c = sext i1 %b to i64
  %d = xor i64 %a, %c
  %r = sub i64 %d, %c
  ret i64 %r
}

define i64 @sext_xor_sub_1(i64 %a, i1 %b) {
; CHECK-LABEL: define i64 @sext_xor_sub_1(
; CHECK-SAME: i64 [[A:%.*]], i1 [[B:%.*]]) {
; CHECK-NEXT:    [[TMP1:%.*]] = sub i64 0, [[A]]
; CHECK-NEXT:    [[R:%.*]] = select i1 [[B]], i64 [[TMP1]], i64 [[A]]
; CHECK-NEXT:    ret i64 [[R]]
;
  %c = sext i1 %b to i64
  %d = xor i64 %c, %a
  %r = sub i64 %d, %c
  ret i64 %r
}

define i64 @sext_xor_sub_2(i64 %a, i1 %b) {
; CHECK-LABEL: define i64 @sext_xor_sub_2(
; CHECK-SAME: i64 [[A:%.*]], i1 [[B:%.*]]) {
; CHECK-NEXT:    [[TMP1:%.*]] = sub i64 0, [[A]]
; CHECK-NEXT:    [[R:%.*]] = select i1 [[B]], i64 [[A]], i64 [[TMP1]]
; CHECK-NEXT:    ret i64 [[R]]
;
  %c = sext i1 %b to i64
  %d = xor i64 %a, %c
  %r = sub i64 %c, %d
  ret i64 %r
}

define i64 @sext_xor_sub_3(i64 %a, i1 %b) {
; CHECK-LABEL: define i64 @sext_xor_sub_3(
; CHECK-SAME: i64 [[A:%.*]], i1 [[B:%.*]]) {
; CHECK-NEXT:    [[TMP1:%.*]] = sub i64 0, [[A]]
; CHECK-NEXT:    [[R:%.*]] = select i1 [[B]], i64 [[A]], i64 [[TMP1]]
; CHECK-NEXT:    ret i64 [[R]]
;
  %c = sext i1 %b to i64
  %d = xor i64 %c, %a
  %r = sub i64 %c, %d
  ret i64 %r
}

; Sext non boolean type.
define i64 @sext_non_bool_xor_sub(i64 %a, i8 %b) {
; CHECK-LABEL: define i64 @sext_non_bool_xor_sub(
; CHECK-SAME: i64 [[A:%.*]], i8 [[B:%.*]]) {
; CHECK-NEXT:    [[C:%.*]] = sext i8 [[B]] to i64
; CHECK-NEXT:    [[D:%.*]] = xor i64 [[A]], [[C]]
; CHECK-NEXT:    [[R:%.*]] = sub i64 [[D]], [[C]]
; CHECK-NEXT:    ret i64 [[R]]
;
  %c = sext i8 %b to i64
  %d = xor i64 %a, %c
  %r = sub i64 %d, %c
  ret i64 %r
}

define i64 @sext_non_bool_xor_sub_1(i64 %a, i8 %b) {
; CHECK-LABEL: define i64 @sext_non_bool_xor_sub_1(
; CHECK-SAME: i64 [[A:%.*]], i8 [[B:%.*]]) {
; CHECK-NEXT:    [[C:%.*]] = sext i8 [[B]] to i64
; CHECK-NEXT:    [[D:%.*]] = xor i64 [[A]], [[C]]
; CHECK-NEXT:    [[R:%.*]] = sub i64 [[D]], [[C]]
; CHECK-NEXT:    ret i64 [[R]]
;
  %c = sext i8 %b to i64
  %d = xor i64 %c, %a
  %r = sub i64 %d, %c
  ret i64 %r
}

; Different boolean values.
define i64 @sext_diff_i1_xor_sub(i64 %a, i1 %b, i1 %c) {
; CHECK-LABEL: define i64 @sext_diff_i1_xor_sub(
; CHECK-SAME: i64 [[A:%.*]], i1 [[B:%.*]], i1 [[C:%.*]]) {
; CHECK-NEXT:    [[D:%.*]] = sext i1 [[B]] to i64
; CHECK-NEXT:    [[E_NEG:%.*]] = zext i1 [[C]] to i64
; CHECK-NEXT:    [[R:%.*]] = add nsw i64 [[E_NEG]], [[D]]
; CHECK-NEXT:    ret i64 [[R]]
;
  %d = sext i1 %b to i64
  %e = sext i1 %c to i64
  %f = xor i64 %a, %d
  %r = sub i64 %d, %e
  ret i64 %r
}

define i64 @sext_diff_i1_xor_sub_1(i64 %a, i1 %b, i1 %c) {
; CHECK-LABEL: define i64 @sext_diff_i1_xor_sub_1(
; CHECK-SAME: i64 [[A:%.*]], i1 [[B:%.*]], i1 [[C:%.*]]) {
; CHECK-NEXT:    [[D:%.*]] = sext i1 [[B]] to i64
; CHECK-NEXT:    [[E_NEG:%.*]] = zext i1 [[C]] to i64
; CHECK-NEXT:    [[R:%.*]] = add nsw i64 [[E_NEG]], [[D]]
; CHECK-NEXT:    ret i64 [[R]]
;
  %d = sext i1 %b to i64
  %e = sext i1 %c to i64
  %f = xor i64 %d, %a
  %r = sub i64 %d, %e
  ret i64 %r
}

; (sext C) has multiple uses.
define i64 @sext_multi_uses(i64 %a, i1 %b, i64 %x) {
; CHECK-LABEL: define i64 @sext_multi_uses(
; CHECK-SAME: i64 [[A:%.*]], i1 [[B:%.*]], i64 [[X:%.*]]) {
; CHECK-NEXT:    [[TMP1:%.*]] = add i64 [[X]], [[A]]
; CHECK-NEXT:    [[TMP2:%.*]] = sub i64 0, [[TMP1]]
; CHECK-NEXT:    [[R:%.*]] = select i1 [[B]], i64 [[TMP2]], i64 [[A]]
; CHECK-NEXT:    ret i64 [[R]]
;
  %c = sext i1 %b to i64
  %d = xor i64 %a, %c
  %e = sub i64 %d, %c
  %f = mul i64 %x, %c
  %r = add i64 %f, %e
  ret i64 %r
}

; (xor X, (sext C)) has multiple uses.
define i64 @xor_multi_uses(i64 %a, i1 %b, i64 %x) {
; CHECK-LABEL: define i64 @xor_multi_uses(
; CHECK-SAME: i64 [[A:%.*]], i1 [[B:%.*]], i64 [[X:%.*]]) {
; CHECK-NEXT:    [[C:%.*]] = sext i1 [[B]] to i64
; CHECK-NEXT:    [[D:%.*]] = xor i64 [[A]], [[C]]
; CHECK-NEXT:    [[E:%.*]] = sub i64 [[D]], [[C]]
; CHECK-NEXT:    [[F:%.*]] = mul i64 [[X]], [[D]]
; CHECK-NEXT:    [[R:%.*]] = add i64 [[F]], [[E]]
; CHECK-NEXT:    ret i64 [[R]]
;
  %c = sext i1 %b to i64
  %d = xor i64 %a, %c
  %e = sub i64 %d, %c
  %f = mul i64 %x, %d
  %r = add i64 %f, %e
  ret i64 %r
}

define i64 @absdiff(i64 %a, i64 %b) {
; CHECK-LABEL: define i64 @absdiff(
; CHECK-SAME: i64 [[A:%.*]], i64 [[B:%.*]]) {
; CHECK-NEXT:    [[C:%.*]] = icmp ult i64 [[A]], [[B]]
; CHECK-NEXT:    [[D:%.*]] = sub i64 [[A]], [[B]]
; CHECK-NEXT:    [[TMP1:%.*]] = sub i64 0, [[D]]
; CHECK-NEXT:    [[RES:%.*]] = select i1 [[C]], i64 [[TMP1]], i64 [[D]]
; CHECK-NEXT:    ret i64 [[RES]]
;
  %c = icmp ult i64 %a, %b
  %c.ext = sext i1 %c to i64
  %d = sub i64 %a, %b
  %may.rev = xor i64 %c.ext, %d
  %res = sub i64 %may.rev, %c.ext
  ret i64 %res
}

; Commuted xor operands compared to absdiff.
define i64 @absdiff1(i64 %a, i64 %b) {
; CHECK-LABEL: define i64 @absdiff1(
; CHECK-SAME: i64 [[A:%.*]], i64 [[B:%.*]]) {
; CHECK-NEXT:    [[C:%.*]] = icmp ult i64 [[A]], [[B]]
; CHECK-NEXT:    [[D:%.*]] = sub i64 [[A]], [[B]]
; CHECK-NEXT:    [[TMP1:%.*]] = sub i64 0, [[D]]
; CHECK-NEXT:    [[RES:%.*]] = select i1 [[C]], i64 [[TMP1]], i64 [[D]]
; CHECK-NEXT:    ret i64 [[RES]]
;
  %c = icmp ult i64 %a, %b
  %c.ext = sext i1 %c to i64
  %d = sub i64 %a, %b
  %may.rev = xor i64 %d, %c.ext
  %res = sub i64 %may.rev, %c.ext
  ret i64 %res
}

; Use ugt as compare cond.
define i64 @absdiff2(i64 %a, i64 %b) {
; CHECK-LABEL: define i64 @absdiff2(
; CHECK-SAME: i64 [[A:%.*]], i64 [[B:%.*]]) {
; CHECK-NEXT:    [[C:%.*]] = icmp ugt i64 [[A]], [[B]]
; CHECK-NEXT:    [[D:%.*]] = sub i64 [[B]], [[A]]
; CHECK-NEXT:    [[TMP1:%.*]] = sub i64 0, [[D]]
; CHECK-NEXT:    [[RES:%.*]] = select i1 [[C]], i64 [[TMP1]], i64 [[D]]
; CHECK-NEXT:    ret i64 [[RES]]
;
  %c = icmp ugt i64 %a, %b
  %c.ext = sext i1 %c to i64
  %d = sub i64 %b, %a
  %may.rev = xor i64 %d, %c.ext
  %res = sub i64 %may.rev, %c.ext
  ret i64 %res
}
