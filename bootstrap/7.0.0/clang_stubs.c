#include "stubgen.h"
#include <clang-c/Index.h>
#include "libclang_extensions.h"
#include <stdio.h>
CAMLprim value
clang_getBuildSessionTimestamp_wrapper()
{
  CAMLparam0();
  unsigned long long result = clang_getBuildSessionTimestamp();
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

DECLARE_OPAQUE(CXVirtualFileOverlay, cxvirtualfileoverlay, Cxvirtualfileoverlay_val, Val_cxvirtualfileoverlay, custom_finalize_default)

CAMLprim value
clang_VirtualFileOverlay_create_wrapper(value options_ocaml)
{
  CAMLparam1(options_ocaml);
  unsigned int options;
  options = Int_val(options_ocaml);
  CXVirtualFileOverlay result = clang_VirtualFileOverlay_create(options);
  {
    CAMLlocal1(data);
    data = Val_cxvirtualfileoverlay(result);
    CAMLreturn(data);
  }
}

enum CXErrorCode
Cxerrorcode_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return CXError_Failure;
  case 1: return CXError_Crashed;
  case 2: return CXError_InvalidArguments;
  case 3: return CXError_ASTReadError;
  }
  failwith_fmt("invalid value for Cxerrorcode_val: %d", Int_val(ocaml));
  return CXError_Failure;
}

value
Val_cxerrorcode(enum CXErrorCode v)
{
  switch (v) {
  case CXError_Failure: return Val_int(0);
  case CXError_Crashed: return Val_int(1);
  case CXError_InvalidArguments: return Val_int(2);
  case CXError_ASTReadError: return Val_int(3);
  case CXError_Success: failwith("unexpected success value");
  }
  failwith_fmt("invalid value for Val_cxerrorcode: %d", v);
  return Val_int(0);
}

CAMLprim value
clang_VirtualFileOverlay_addFileMapping_wrapper(value arg_ocaml, value virtualPath_ocaml, value realPath_ocaml)
{
  CAMLparam3(arg_ocaml, virtualPath_ocaml, realPath_ocaml);
  CXVirtualFileOverlay arg;
  arg = Cxvirtualfileoverlay_val(arg_ocaml);
  const char * virtualPath;
  virtualPath = String_val(virtualPath_ocaml);
  const char * realPath;
  realPath = String_val(realPath_ocaml);
  enum CXErrorCode result = clang_VirtualFileOverlay_addFileMapping(arg, virtualPath, realPath);
  if (result == CXError_Success) {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 0);
    data = Val_unit;
    Store_field(ocaml_result, 0, data);
    CAMLreturn(ocaml_result);
  }
  else {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 1);
    data = Val_cxerrorcode(result);
    Store_field(ocaml_result, 0, data);
    
    CAMLreturn(ocaml_result);
  }}

CAMLprim value
clang_VirtualFileOverlay_setCaseSensitivity_wrapper(value arg_ocaml, value caseSensitive_ocaml)
{
  CAMLparam2(arg_ocaml, caseSensitive_ocaml);
  CXVirtualFileOverlay arg;
  arg = Cxvirtualfileoverlay_val(arg_ocaml);
  int caseSensitive;
  caseSensitive = Int_val(caseSensitive_ocaml);
  enum CXErrorCode result = clang_VirtualFileOverlay_setCaseSensitivity(arg, caseSensitive);
  if (result == CXError_Success) {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 0);
    data = Val_unit;
    Store_field(ocaml_result, 0, data);
    CAMLreturn(ocaml_result);
  }
  else {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 1);
    data = Val_cxerrorcode(result);
    Store_field(ocaml_result, 0, data);
    
    CAMLreturn(ocaml_result);
  }}

DECLARE_OPAQUE(CXModuleMapDescriptor, cxmodulemapdescriptor, Cxmodulemapdescriptor_val, Val_cxmodulemapdescriptor, custom_finalize_default)

CAMLprim value
clang_ModuleMapDescriptor_create_wrapper(value options_ocaml)
{
  CAMLparam1(options_ocaml);
  unsigned int options;
  options = Int_val(options_ocaml);
  CXModuleMapDescriptor result = clang_ModuleMapDescriptor_create(options);
  {
    CAMLlocal1(data);
    data = Val_cxmodulemapdescriptor(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ModuleMapDescriptor_setFrameworkModuleName_wrapper(value arg_ocaml, value name_ocaml)
{
  CAMLparam2(arg_ocaml, name_ocaml);
  CXModuleMapDescriptor arg;
  arg = Cxmodulemapdescriptor_val(arg_ocaml);
  const char * name;
  name = String_val(name_ocaml);
  enum CXErrorCode result = clang_ModuleMapDescriptor_setFrameworkModuleName(arg, name);
  if (result == CXError_Success) {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 0);
    data = Val_unit;
    Store_field(ocaml_result, 0, data);
    CAMLreturn(ocaml_result);
  }
  else {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 1);
    data = Val_cxerrorcode(result);
    Store_field(ocaml_result, 0, data);
    
    CAMLreturn(ocaml_result);
  }}

CAMLprim value
clang_ModuleMapDescriptor_setUmbrellaHeader_wrapper(value arg_ocaml, value name_ocaml)
{
  CAMLparam2(arg_ocaml, name_ocaml);
  CXModuleMapDescriptor arg;
  arg = Cxmodulemapdescriptor_val(arg_ocaml);
  const char * name;
  name = String_val(name_ocaml);
  enum CXErrorCode result = clang_ModuleMapDescriptor_setUmbrellaHeader(arg, name);
  if (result == CXError_Success) {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 0);
    data = Val_unit;
    Store_field(ocaml_result, 0, data);
    CAMLreturn(ocaml_result);
  }
  else {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 1);
    data = Val_cxerrorcode(result);
    Store_field(ocaml_result, 0, data);
    
    CAMLreturn(ocaml_result);
  }}

CAMLprim value
clang_ModuleMapDescriptor_writeToBuffer_wrapper(value arg_ocaml, value options_ocaml)
{
  CAMLparam2(arg_ocaml, options_ocaml);
  CXModuleMapDescriptor arg;
  arg = Cxmodulemapdescriptor_val(arg_ocaml);
  unsigned int options;
  options = Int_val(options_ocaml);
  unsigned int out_buffer_size;
  char * out_buffer_ptr;
  enum CXErrorCode result = clang_ModuleMapDescriptor_writeToBuffer(arg, options, &out_buffer_ptr, &out_buffer_size);
  if (result == CXError_Success) {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 0);
    data = caml_alloc_initialized_string(out_buffer_size, out_buffer_ptr);
clang_free(out_buffer_ptr);
    Store_field(ocaml_result, 0, data);
    CAMLreturn(ocaml_result);
  }
  else {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 1);
    data = Val_cxerrorcode(result);
    Store_field(ocaml_result, 0, data);
    
    CAMLreturn(ocaml_result);
  }}

static void finalize_cxindex(value v) {
  clang_disposeIndex(*((CXIndex *) Data_custom_val(v)));;
}
DECLARE_OPAQUE(CXIndex, cxindex, Cxindex_val, Val_cxindex, finalize_cxindex)

CAMLprim value
clang_createIndex_wrapper(value excludeDeclarationsFromPCH_ocaml, value displayDiagnostics_ocaml)
{
  CAMLparam2(excludeDeclarationsFromPCH_ocaml, displayDiagnostics_ocaml);
  int excludeDeclarationsFromPCH;
  excludeDeclarationsFromPCH = Bool_val(excludeDeclarationsFromPCH_ocaml);
  int displayDiagnostics;
  displayDiagnostics = Bool_val(displayDiagnostics_ocaml);
  CXIndex result = clang_createIndex(excludeDeclarationsFromPCH, displayDiagnostics);
  {
    CAMLlocal1(data);
    data = Val_cxindex(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_CXIndex_setGlobalOptions_wrapper(value arg_ocaml, value options_ocaml)
{
  CAMLparam2(arg_ocaml, options_ocaml);
  CXIndex arg;
  arg = Cxindex_val(arg_ocaml);
  unsigned int options;
  options = Int_val(options_ocaml);
  clang_CXIndex_setGlobalOptions(arg, options);
  CAMLreturn(Val_unit);
}

CAMLprim value
clang_CXIndex_getGlobalOptions_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXIndex arg;
  arg = Cxindex_val(arg_ocaml);
  unsigned int result = clang_CXIndex_getGlobalOptions(arg);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_CXIndex_setInvocationEmissionPathOption_wrapper(value arg_ocaml, value Path_ocaml)
{
  CAMLparam2(arg_ocaml, Path_ocaml);
  CXIndex arg;
  arg = Cxindex_val(arg_ocaml);
  const char * Path;
  Path = String_val(Path_ocaml);
  clang_CXIndex_setInvocationEmissionPathOption(arg, Path);
  CAMLreturn(Val_unit);
}

DECLARE_OPAQUE(CXFile, cxfile, Cxfile_val, Val_cxfile, custom_finalize_default)

CAMLprim value
clang_getFileName_wrapper(value SFile_ocaml)
{
  CAMLparam1(SFile_ocaml);
  CXFile SFile;
  SFile = Cxfile_val(SFile_ocaml);
  CXString result = clang_getFileName(SFile);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getFileTime_wrapper(value SFile_ocaml)
{
  CAMLparam1(SFile_ocaml);
  CXFile SFile;
  SFile = Cxfile_val(SFile_ocaml);
  time_t result = clang_getFileTime(SFile);
  {
    CAMLlocal1(data);
    data = Val_int((int) result);
    CAMLreturn(data);
  }
}

static void finalize_cxtranslationunit(value v) {
  clang_disposeTranslationUnit(*((CXTranslationUnit *) Data_custom_val(v)));;
}
DECLARE_OPAQUE(CXTranslationUnit, cxtranslationunit, Cxtranslationunit_val, Val_cxtranslationunit, finalize_cxtranslationunit)

CAMLprim value
clang_isFileMultipleIncludeGuarded_wrapper(value tu_ocaml, value file_ocaml)
{
  CAMLparam2(tu_ocaml, file_ocaml);
  CXTranslationUnit tu;
  tu = Cxtranslationunit_val(Field(tu_ocaml, 0));
  CXFile file;
  file = Cxfile_val(file_ocaml);
  unsigned int result = clang_isFileMultipleIncludeGuarded(tu, file);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getFile_wrapper(value tu_ocaml, value file_name_ocaml)
{
  CAMLparam2(tu_ocaml, file_name_ocaml);
  CXTranslationUnit tu;
  tu = Cxtranslationunit_val(Field(tu_ocaml, 0));
  const char * file_name;
  file_name = String_val(file_name_ocaml);
  CXFile result = clang_getFile(tu, file_name);
  {
    CAMLlocal1(data);
    data = Val_cxfile(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getFileContents_wrapper(value tu_ocaml, value file_ocaml)
{
  CAMLparam2(tu_ocaml, file_ocaml);
  CXTranslationUnit tu;
  tu = Cxtranslationunit_val(Field(tu_ocaml, 0));
  CXFile file;
  file = Cxfile_val(file_ocaml);
  size_t size;
  const char * result = clang_getFileContents(tu, file, &size);
  {
    CAMLlocal1(data);
      if (result == NULL) {
    data = Val_int(0);
  }
  else {
    data = caml_alloc(1, 0);
    Store_field(data, 0, caml_alloc_initialized_string(size, result));
    
  };

    CAMLreturn(data);
  }
}

CAMLprim value
clang_File_isEqual_wrapper(value file1_ocaml, value file2_ocaml)
{
  CAMLparam2(file1_ocaml, file2_ocaml);
  CXFile file1;
  file1 = Cxfile_val(file1_ocaml);
  CXFile file2;
  file2 = Cxfile_val(file2_ocaml);
  int result = clang_File_isEqual(file1, file2);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_File_tryGetRealPathName_wrapper(value file_ocaml)
{
  CAMLparam1(file_ocaml);
  CXFile file;
  file = Cxfile_val(file_ocaml);
  CXString result = clang_File_tryGetRealPathName(file);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

DECLARE_OPAQUE(CXSourceLocation, cxsourcelocation, Cxsourcelocation_val, Val_cxsourcelocation, custom_finalize_default)

CAMLprim value
clang_getNullLocation_wrapper()
{
  CAMLparam0();
  CXSourceLocation result = clang_getNullLocation();
  {
    CAMLlocal1(data);
    data = Val_cxsourcelocation(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_equalLocations_wrapper(value loc1_ocaml, value loc2_ocaml)
{
  CAMLparam2(loc1_ocaml, loc2_ocaml);
  CXSourceLocation loc1;
  loc1 = Cxsourcelocation_val(loc1_ocaml);
  CXSourceLocation loc2;
  loc2 = Cxsourcelocation_val(loc2_ocaml);
  unsigned int result = clang_equalLocations(loc1, loc2);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getLocation_wrapper(value tu_ocaml, value file_ocaml, value line_ocaml, value column_ocaml)
{
  CAMLparam4(tu_ocaml, file_ocaml, line_ocaml, column_ocaml);
  CXTranslationUnit tu;
  tu = Cxtranslationunit_val(Field(tu_ocaml, 0));
  CXFile file;
  file = Cxfile_val(file_ocaml);
  unsigned int line;
  line = Int_val(line_ocaml);
  unsigned int column;
  column = Int_val(column_ocaml);
  CXSourceLocation result = clang_getLocation(tu, file, line, column);
  {
    CAMLlocal1(data);
    data = Val_cxsourcelocation(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getLocationForOffset_wrapper(value tu_ocaml, value file_ocaml, value offset_ocaml)
{
  CAMLparam3(tu_ocaml, file_ocaml, offset_ocaml);
  CXTranslationUnit tu;
  tu = Cxtranslationunit_val(Field(tu_ocaml, 0));
  CXFile file;
  file = Cxfile_val(file_ocaml);
  unsigned int offset;
  offset = Int_val(offset_ocaml);
  CXSourceLocation result = clang_getLocationForOffset(tu, file, offset);
  {
    CAMLlocal1(data);
    data = Val_cxsourcelocation(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Location_isInSystemHeader_wrapper(value location_ocaml)
{
  CAMLparam1(location_ocaml);
  CXSourceLocation location;
  location = Cxsourcelocation_val(location_ocaml);
  int result = clang_Location_isInSystemHeader(location);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Location_isFromMainFile_wrapper(value location_ocaml)
{
  CAMLparam1(location_ocaml);
  CXSourceLocation location;
  location = Cxsourcelocation_val(location_ocaml);
  int result = clang_Location_isFromMainFile(location);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

DECLARE_OPAQUE(CXSourceRange, cxsourcerange, Cxsourcerange_val, Val_cxsourcerange, custom_finalize_default)

CAMLprim value
clang_getNullRange_wrapper()
{
  CAMLparam0();
  CXSourceRange result = clang_getNullRange();
  {
    CAMLlocal1(data);
    data = Val_cxsourcerange(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getRange_wrapper(value begin_ocaml, value end_ocaml)
{
  CAMLparam2(begin_ocaml, end_ocaml);
  CXSourceLocation begin;
  begin = Cxsourcelocation_val(begin_ocaml);
  CXSourceLocation end;
  end = Cxsourcelocation_val(end_ocaml);
  CXSourceRange result = clang_getRange(begin, end);
  {
    CAMLlocal1(data);
    data = Val_cxsourcerange(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_equalRanges_wrapper(value range1_ocaml, value range2_ocaml)
{
  CAMLparam2(range1_ocaml, range2_ocaml);
  CXSourceRange range1;
  range1 = Cxsourcerange_val(range1_ocaml);
  CXSourceRange range2;
  range2 = Cxsourcerange_val(range2_ocaml);
  unsigned int result = clang_equalRanges(range1, range2);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Range_isNull_wrapper(value range_ocaml)
{
  CAMLparam1(range_ocaml);
  CXSourceRange range;
  range = Cxsourcerange_val(range_ocaml);
  int result = clang_Range_isNull(range);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getExpansionLocation_wrapper(value location_ocaml)
{
  CAMLparam1(location_ocaml);
  CXSourceLocation location;
  location = Cxsourcelocation_val(location_ocaml);
  CXFile file;
  unsigned int line;
  unsigned int column;
  unsigned int offset;
  clang_getExpansionLocation(location, &file, &line, &column, &offset);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(4);
  {
    CAMLlocal1(field);
    field = Val_cxfile(file);
    Store_field(data, 0, field);
  }
  {
    CAMLlocal1(field);
    field = Val_int(line);
    Store_field(data, 1, field);
  }
  {
    CAMLlocal1(field);
    field = Val_int(column);
    Store_field(data, 2, field);
  }
  {
    CAMLlocal1(field);
    field = Val_int(offset);
    Store_field(data, 3, field);
  }

    CAMLreturn(data);
  }
}

CAMLprim value
clang_getPresumedLocation_wrapper(value location_ocaml)
{
  CAMLparam1(location_ocaml);
  CXSourceLocation location;
  location = Cxsourcelocation_val(location_ocaml);
  CXString filename;
  unsigned int line;
  unsigned int column;
  clang_getPresumedLocation(location, &filename, &line, &column);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(3);
  {
    CAMLlocal1(field);
    field = caml_copy_string(clang_getCString(filename));
                    clang_disposeString(filename);
    Store_field(data, 0, field);
  }
  {
    CAMLlocal1(field);
    field = Val_int(line);
    Store_field(data, 1, field);
  }
  {
    CAMLlocal1(field);
    field = Val_int(column);
    Store_field(data, 2, field);
  }

    CAMLreturn(data);
  }
}

CAMLprim value
clang_getInstantiationLocation_wrapper(value location_ocaml)
{
  CAMLparam1(location_ocaml);
  CXSourceLocation location;
  location = Cxsourcelocation_val(location_ocaml);
  CXFile file;
  unsigned int line;
  unsigned int column;
  unsigned int offset;
  clang_getInstantiationLocation(location, &file, &line, &column, &offset);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(4);
  {
    CAMLlocal1(field);
    field = Val_cxfile(file);
    Store_field(data, 0, field);
  }
  {
    CAMLlocal1(field);
    field = Val_int(line);
    Store_field(data, 1, field);
  }
  {
    CAMLlocal1(field);
    field = Val_int(column);
    Store_field(data, 2, field);
  }
  {
    CAMLlocal1(field);
    field = Val_int(offset);
    Store_field(data, 3, field);
  }

    CAMLreturn(data);
  }
}

CAMLprim value
clang_getSpellingLocation_wrapper(value location_ocaml)
{
  CAMLparam1(location_ocaml);
  CXSourceLocation location;
  location = Cxsourcelocation_val(location_ocaml);
  CXFile file;
  unsigned int line;
  unsigned int column;
  unsigned int offset;
  clang_getSpellingLocation(location, &file, &line, &column, &offset);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(4);
  {
    CAMLlocal1(field);
    field = Val_cxfile(file);
    Store_field(data, 0, field);
  }
  {
    CAMLlocal1(field);
    field = Val_int(line);
    Store_field(data, 1, field);
  }
  {
    CAMLlocal1(field);
    field = Val_int(column);
    Store_field(data, 2, field);
  }
  {
    CAMLlocal1(field);
    field = Val_int(offset);
    Store_field(data, 3, field);
  }

    CAMLreturn(data);
  }
}

CAMLprim value
clang_getFileLocation_wrapper(value location_ocaml)
{
  CAMLparam1(location_ocaml);
  CXSourceLocation location;
  location = Cxsourcelocation_val(location_ocaml);
  CXFile file;
  unsigned int line;
  unsigned int column;
  unsigned int offset;
  clang_getFileLocation(location, &file, &line, &column, &offset);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(4);
  {
    CAMLlocal1(field);
    field = Val_cxfile(file);
    Store_field(data, 0, field);
  }
  {
    CAMLlocal1(field);
    field = Val_int(line);
    Store_field(data, 1, field);
  }
  {
    CAMLlocal1(field);
    field = Val_int(column);
    Store_field(data, 2, field);
  }
  {
    CAMLlocal1(field);
    field = Val_int(offset);
    Store_field(data, 3, field);
  }

    CAMLreturn(data);
  }
}

CAMLprim value
clang_getRangeStart_wrapper(value range_ocaml)
{
  CAMLparam1(range_ocaml);
  CXSourceRange range;
  range = Cxsourcerange_val(range_ocaml);
  CXSourceLocation result = clang_getRangeStart(range);
  {
    CAMLlocal1(data);
    data = Val_cxsourcelocation(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getRangeEnd_wrapper(value range_ocaml)
{
  CAMLparam1(range_ocaml);
  CXSourceRange range;
  range = Cxsourcerange_val(range_ocaml);
  CXSourceLocation result = clang_getRangeEnd(range);
  {
    CAMLlocal1(data);
    data = Val_cxsourcelocation(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getSkippedRanges_wrapper(value tu_ocaml, value file_ocaml)
{
  CAMLparam2(tu_ocaml, file_ocaml);
  CXTranslationUnit tu;
  tu = Cxtranslationunit_val(Field(tu_ocaml, 0));
  CXFile file;
  file = Cxfile_val(file_ocaml);
  CXSourceRangeList * result = clang_getSkippedRanges(tu, file);
  {
    CAMLlocal1(data);
    
data = caml_alloc(result->count, 0);
for (unsigned int i = 0; i < result->count; i++) {
  CAMLlocal1(field);
  field = Val_cxsourcerange(result->ranges[i]);
  Store_field(data, i, field);
}
clang_disposeSourceRangeList(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getAllSkippedRanges_wrapper(value tu_ocaml)
{
  CAMLparam1(tu_ocaml);
  CXTranslationUnit tu;
  tu = Cxtranslationunit_val(Field(tu_ocaml, 0));
  CXSourceRangeList * result = clang_getAllSkippedRanges(tu);
  {
    CAMLlocal1(data);
    
data = caml_alloc(result->count, 0);
for (unsigned int i = 0; i < result->count; i++) {
  CAMLlocal1(field);
  field = Val_cxsourcerange(result->ranges[i]);
  Store_field(data, i, field);
}
clang_disposeSourceRangeList(result);
    CAMLreturn(data);
  }
}

DECLARE_OPAQUE(CXDiagnosticSet, cxdiagnosticset, Cxdiagnosticset_val, Val_cxdiagnosticset, custom_finalize_default)

CAMLprim value
clang_getNumDiagnosticsInSet_wrapper(value Diags_ocaml)
{
  CAMLparam1(Diags_ocaml);
  CXDiagnosticSet Diags;
  Diags = Cxdiagnosticset_val(Diags_ocaml);
  unsigned int result = clang_getNumDiagnosticsInSet(Diags);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

DECLARE_OPAQUE(CXDiagnostic, cxdiagnostic, Cxdiagnostic_val, Val_cxdiagnostic, custom_finalize_default)

CAMLprim value
clang_getDiagnosticInSet_wrapper(value Diags_ocaml, value Index_ocaml)
{
  CAMLparam2(Diags_ocaml, Index_ocaml);
  CXDiagnosticSet Diags;
  Diags = Cxdiagnosticset_val(Diags_ocaml);
  unsigned int Index;
  Index = Int_val(Index_ocaml);
  CXDiagnostic result = clang_getDiagnosticInSet(Diags, Index);
  {
    CAMLlocal1(data);
    data = Val_cxdiagnostic(result);
    CAMLreturn(data);
  }
}

enum CXLoadDiag_Error
Cxloaddiag_error_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return CXLoadDiag_Unknown;
  case 1: return CXLoadDiag_CannotLoad;
  case 2: return CXLoadDiag_InvalidFile;
  }
  failwith_fmt("invalid value for Cxloaddiag_error_val: %d", Int_val(ocaml));
  return CXLoadDiag_Unknown;
}

value
Val_cxloaddiag_error(enum CXLoadDiag_Error v)
{
  switch (v) {
  case CXLoadDiag_Unknown: return Val_int(0);
  case CXLoadDiag_CannotLoad: return Val_int(1);
  case CXLoadDiag_InvalidFile: return Val_int(2);
  case CXLoadDiag_None: failwith("unexpected success value");
  }
  failwith_fmt("invalid value for Val_cxloaddiag_error: %d", v);
  return Val_int(0);
}

CAMLprim value
clang_loadDiagnostics_wrapper(value file_ocaml)
{
  CAMLparam1(file_ocaml);
  const char * file;
  file = String_val(file_ocaml);
  enum CXLoadDiag_Error error;
  CXString errorString;
  CXDiagnosticSet result = clang_loadDiagnostics(file, &error, &errorString);
  if (error == CXLoadDiag_None) {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 0);
    data = Val_cxdiagnosticset(result);
    Store_field(ocaml_result, 0, data);
    CAMLreturn(ocaml_result);
  }
  else {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 1);
    data = caml_alloc_tuple(2);
  {
    CAMLlocal1(field);
    field = Val_cxloaddiag_error(error);
    Store_field(data, 0, field);
  }
  {
    CAMLlocal1(field);
    field = caml_copy_string(clang_getCString(errorString));
                    clang_disposeString(errorString);
    Store_field(data, 1, field);
  }

    Store_field(ocaml_result, 0, data);
    
    CAMLreturn(ocaml_result);
  }}

CAMLprim value
clang_getChildDiagnostics_wrapper(value D_ocaml)
{
  CAMLparam1(D_ocaml);
  CXDiagnostic D;
  D = Cxdiagnostic_val(D_ocaml);
  CXDiagnosticSet result = clang_getChildDiagnostics(D);
  {
    CAMLlocal1(data);
    data = Val_cxdiagnosticset(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getNumDiagnostics_wrapper(value Unit_ocaml)
{
  CAMLparam1(Unit_ocaml);
  CXTranslationUnit Unit;
  Unit = Cxtranslationunit_val(Field(Unit_ocaml, 0));
  unsigned int result = clang_getNumDiagnostics(Unit);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getDiagnostic_wrapper(value Unit_ocaml, value Index_ocaml)
{
  CAMLparam2(Unit_ocaml, Index_ocaml);
  CXTranslationUnit Unit;
  Unit = Cxtranslationunit_val(Field(Unit_ocaml, 0));
  unsigned int Index;
  Index = Int_val(Index_ocaml);
  CXDiagnostic result = clang_getDiagnostic(Unit, Index);
  {
    CAMLlocal1(data);
    data = Val_cxdiagnostic(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getDiagnosticSetFromTU_wrapper(value Unit_ocaml)
{
  CAMLparam1(Unit_ocaml);
  CXTranslationUnit Unit;
  Unit = Cxtranslationunit_val(Field(Unit_ocaml, 0));
  CXDiagnosticSet result = clang_getDiagnosticSetFromTU(Unit);
  {
    CAMLlocal1(data);
    data = Val_cxdiagnosticset(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_formatDiagnostic_wrapper(value Diagnostic_ocaml, value Options_ocaml)
{
  CAMLparam2(Diagnostic_ocaml, Options_ocaml);
  CXDiagnostic Diagnostic;
  Diagnostic = Cxdiagnostic_val(Diagnostic_ocaml);
  unsigned int Options;
  Options = Int_val(Options_ocaml);
  CXString result = clang_formatDiagnostic(Diagnostic, Options);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_defaultDiagnosticDisplayOptions_wrapper()
{
  CAMLparam0();
  unsigned int result = clang_defaultDiagnosticDisplayOptions();
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

enum CXDiagnosticSeverity
Cxdiagnosticseverity_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return CXDiagnostic_Ignored;
  case 1: return CXDiagnostic_Note;
  case 2: return CXDiagnostic_Warning;
  case 3: return CXDiagnostic_Error;
  case 4: return CXDiagnostic_Fatal;
  }
  failwith_fmt("invalid value for Cxdiagnosticseverity_val: %d", Int_val(ocaml));
  return CXDiagnostic_Ignored;
}

value
Val_cxdiagnosticseverity(enum CXDiagnosticSeverity v)
{
  switch (v) {
  case CXDiagnostic_Ignored: return Val_int(0);
  case CXDiagnostic_Note: return Val_int(1);
  case CXDiagnostic_Warning: return Val_int(2);
  case CXDiagnostic_Error: return Val_int(3);
  case CXDiagnostic_Fatal: return Val_int(4);
  }
  failwith_fmt("invalid value for Val_cxdiagnosticseverity: %d", v);
  return Val_int(0);
}

CAMLprim value
clang_getDiagnosticSeverity_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXDiagnostic arg;
  arg = Cxdiagnostic_val(arg_ocaml);
  enum CXDiagnosticSeverity result = clang_getDiagnosticSeverity(arg);
  {
    CAMLlocal1(data);
    data = Val_cxdiagnosticseverity(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getDiagnosticLocation_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXDiagnostic arg;
  arg = Cxdiagnostic_val(arg_ocaml);
  CXSourceLocation result = clang_getDiagnosticLocation(arg);
  {
    CAMLlocal1(data);
    data = Val_cxsourcelocation(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getDiagnosticSpelling_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXDiagnostic arg;
  arg = Cxdiagnostic_val(arg_ocaml);
  CXString result = clang_getDiagnosticSpelling(arg);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getDiagnosticOption_wrapper(value Diag_ocaml)
{
  CAMLparam1(Diag_ocaml);
  CXDiagnostic Diag;
  Diag = Cxdiagnostic_val(Diag_ocaml);
  CXString Disable;
  CXString result = clang_getDiagnosticOption(Diag, &Disable);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  {
    CAMLlocal1(field);
    field = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    Store_field(data, 0, field);
  }
  {
    CAMLlocal1(field);
    field = caml_copy_string(clang_getCString(Disable));
                    clang_disposeString(Disable);
    Store_field(data, 1, field);
  }

    CAMLreturn(data);
  }
}

CAMLprim value
clang_getDiagnosticCategory_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXDiagnostic arg;
  arg = Cxdiagnostic_val(arg_ocaml);
  unsigned int result = clang_getDiagnosticCategory(arg);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getDiagnosticCategoryText_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXDiagnostic arg;
  arg = Cxdiagnostic_val(arg_ocaml);
  CXString result = clang_getDiagnosticCategoryText(arg);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getDiagnosticNumRanges_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXDiagnostic arg;
  arg = Cxdiagnostic_val(arg_ocaml);
  unsigned int result = clang_getDiagnosticNumRanges(arg);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getDiagnosticRange_wrapper(value Diagnostic_ocaml, value Range_ocaml)
{
  CAMLparam2(Diagnostic_ocaml, Range_ocaml);
  CXDiagnostic Diagnostic;
  Diagnostic = Cxdiagnostic_val(Diagnostic_ocaml);
  unsigned int Range;
  Range = Int_val(Range_ocaml);
  CXSourceRange result = clang_getDiagnosticRange(Diagnostic, Range);
  {
    CAMLlocal1(data);
    data = Val_cxsourcerange(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getDiagnosticNumFixIts_wrapper(value Diagnostic_ocaml)
{
  CAMLparam1(Diagnostic_ocaml);
  CXDiagnostic Diagnostic;
  Diagnostic = Cxdiagnostic_val(Diagnostic_ocaml);
  unsigned int result = clang_getDiagnosticNumFixIts(Diagnostic);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getDiagnosticFixIt_wrapper(value Diagnostic_ocaml, value FixIt_ocaml, value ReplacementRange_ocaml)
{
  CAMLparam3(Diagnostic_ocaml, FixIt_ocaml, ReplacementRange_ocaml);
  CXDiagnostic Diagnostic;
  Diagnostic = Cxdiagnostic_val(Diagnostic_ocaml);
  unsigned int FixIt;
  FixIt = Int_val(FixIt_ocaml);
  CXSourceRange ReplacementRange;
  ReplacementRange = Cxsourcerange_val(ReplacementRange_ocaml);
  CXString result = clang_getDiagnosticFixIt(Diagnostic, FixIt, &ReplacementRange);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  {
    CAMLlocal1(field);
    field = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    Store_field(data, 0, field);
  }
  {
    CAMLlocal1(field);
    field = Val_cxsourcerange(ReplacementRange);
    Store_field(data, 1, field);
  }

    CAMLreturn(data);
  }
}

CAMLprim value
clang_getTranslationUnitSpelling_wrapper(value CTUnit_ocaml)
{
  CAMLparam1(CTUnit_ocaml);
  CXTranslationUnit CTUnit;
  CTUnit = Cxtranslationunit_val(Field(CTUnit_ocaml, 0));
  CXString result = clang_getTranslationUnitSpelling(CTUnit);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

static value __attribute__((unused))
Val_cxunsavedfile(struct CXUnsavedFile v)
{
  CAMLparam0();
  CAMLlocal2(ocaml, string);
  ocaml = caml_alloc_tuple(2);
  {
     CAMLlocal1(data);
     data = caml_copy_string(v.Filename);
     Store_field(ocaml, 0, data);
  }
string = caml_alloc_string(v.Length);
  memcpy(String_val(string), v.Contents, v.Length);
  Store_field(ocaml, 1, string);
CAMLreturn(ocaml);
}

static struct CXUnsavedFile __attribute__((unused))
Cxunsavedfile_val(value ocaml)
{
  CAMLparam1(ocaml);
  struct CXUnsavedFile v;
{
                      const char * data;
                      data = String_val(Field(ocaml, 0));
                      v.Filename = data;
                              }
v.Length = caml_string_length(Field(ocaml, 1));
  v.Contents = String_val(Field(ocaml, 1));
CAMLreturnT(struct CXUnsavedFile, v);
}
CAMLprim value
clang_createTranslationUnitFromSourceFile_wrapper(value CIdx_ocaml, value source_filename_ocaml, value clang_command_line_args_ocaml, value unsaved_files_ocaml)
{
  CAMLparam4(CIdx_ocaml, source_filename_ocaml, clang_command_line_args_ocaml, unsaved_files_ocaml);
  CXIndex CIdx;
  CIdx = Cxindex_val(CIdx_ocaml);
  const char * source_filename;
  source_filename = String_val(source_filename_ocaml);
  int num_clang_command_line_args = Wosize_val(clang_command_line_args_ocaml);
   char * * clang_command_line_args = xmalloc(num_clang_command_line_args * sizeof(const char *const));
  int i; for (i = 0; i < num_clang_command_line_args; i++) {
    clang_command_line_args[i] = String_val(Field(clang_command_line_args_ocaml, i));
  }
  unsigned int num_unsaved_files = Wosize_val(unsaved_files_ocaml);
  struct CXUnsavedFile * unsaved_files = xmalloc(num_unsaved_files * sizeof(struct CXUnsavedFile));
  unsigned int i2; for (i2 = 0; i2 < num_unsaved_files; i2++) {
    unsaved_files[i2] = Cxunsavedfile_val(Field(unsaved_files_ocaml, i2));
  }
  CXTranslationUnit result = clang_createTranslationUnitFromSourceFile(CIdx, source_filename, num_clang_command_line_args, (const char *const *) clang_command_line_args, num_unsaved_files, (struct CXUnsavedFile *) unsaved_files);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxtranslationunit(result));
  Store_field(data, 1, CIdx_ocaml);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_createTranslationUnit_wrapper(value CIdx_ocaml, value ast_filename_ocaml)
{
  CAMLparam2(CIdx_ocaml, ast_filename_ocaml);
  CXIndex CIdx;
  CIdx = Cxindex_val(CIdx_ocaml);
  const char * ast_filename;
  ast_filename = String_val(ast_filename_ocaml);
  CXTranslationUnit result = clang_createTranslationUnit(CIdx, ast_filename);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxtranslationunit(result));
  Store_field(data, 1, CIdx_ocaml);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_createTranslationUnit2_wrapper(value CIdx_ocaml, value ast_filename_ocaml)
{
  CAMLparam2(CIdx_ocaml, ast_filename_ocaml);
  CXIndex CIdx;
  CIdx = Cxindex_val(CIdx_ocaml);
  const char * ast_filename;
  ast_filename = String_val(ast_filename_ocaml);
  CXTranslationUnit out_TU;
  enum CXErrorCode result = clang_createTranslationUnit2(CIdx, ast_filename, &out_TU);
  if (result == CXError_Success) {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 0);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxtranslationunit(out_TU));
  Store_field(data, 1, CIdx_ocaml);
    Store_field(ocaml_result, 0, data);
    CAMLreturn(ocaml_result);
  }
  else {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 1);
    data = Val_cxerrorcode(result);
    Store_field(ocaml_result, 0, data);
    
    CAMLreturn(ocaml_result);
  }}

CAMLprim value
clang_defaultEditingTranslationUnitOptions_wrapper()
{
  CAMLparam0();
  unsigned int result = clang_defaultEditingTranslationUnitOptions();
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_parseTranslationUnit_wrapper(value CIdx_ocaml, value source_filename_ocaml, value command_line_args_ocaml, value unsaved_files_ocaml, value options_ocaml)
{
  CAMLparam5(CIdx_ocaml, source_filename_ocaml, command_line_args_ocaml, unsaved_files_ocaml, options_ocaml);
  CXIndex CIdx;
  CIdx = Cxindex_val(CIdx_ocaml);
  const char * source_filename;
  source_filename = String_val(source_filename_ocaml);
  int num_command_line_args = Wosize_val(command_line_args_ocaml);
   char * * command_line_args = xmalloc(num_command_line_args * sizeof(const char *const));
  int i; for (i = 0; i < num_command_line_args; i++) {
    command_line_args[i] = String_val(Field(command_line_args_ocaml, i));
  }
  unsigned int num_unsaved_files = Wosize_val(unsaved_files_ocaml);
  struct CXUnsavedFile * unsaved_files = xmalloc(num_unsaved_files * sizeof(struct CXUnsavedFile));
  unsigned int i2; for (i2 = 0; i2 < num_unsaved_files; i2++) {
    unsaved_files[i2] = Cxunsavedfile_val(Field(unsaved_files_ocaml, i2));
  }
  unsigned int options;
  options = Int_val(options_ocaml);
  CXTranslationUnit result = clang_parseTranslationUnit(CIdx, source_filename, (const char *const *) command_line_args, num_command_line_args, (struct CXUnsavedFile *) unsaved_files, num_unsaved_files, options);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxtranslationunit(result));
  Store_field(data, 1, CIdx_ocaml);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_parseTranslationUnit2_wrapper(value CIdx_ocaml, value source_filename_ocaml, value command_line_args_ocaml, value unsaved_files_ocaml, value options_ocaml)
{
  CAMLparam5(CIdx_ocaml, source_filename_ocaml, command_line_args_ocaml, unsaved_files_ocaml, options_ocaml);
  CXIndex CIdx;
  CIdx = Cxindex_val(CIdx_ocaml);
  const char * source_filename;
  source_filename = String_val(source_filename_ocaml);
  int num_command_line_args = Wosize_val(command_line_args_ocaml);
   char * * command_line_args = xmalloc(num_command_line_args * sizeof(const char *const));
  int i; for (i = 0; i < num_command_line_args; i++) {
    command_line_args[i] = String_val(Field(command_line_args_ocaml, i));
  }
  unsigned int num_unsaved_files = Wosize_val(unsaved_files_ocaml);
  struct CXUnsavedFile * unsaved_files = xmalloc(num_unsaved_files * sizeof(struct CXUnsavedFile));
  unsigned int i2; for (i2 = 0; i2 < num_unsaved_files; i2++) {
    unsaved_files[i2] = Cxunsavedfile_val(Field(unsaved_files_ocaml, i2));
  }
  unsigned int options;
  options = Int_val(options_ocaml);
  CXTranslationUnit out_TU;
  enum CXErrorCode result = clang_parseTranslationUnit2(CIdx, source_filename, (const char *const *) command_line_args, num_command_line_args, (struct CXUnsavedFile *) unsaved_files, num_unsaved_files, options, &out_TU);
  if (result == CXError_Success) {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 0);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxtranslationunit(out_TU));
  Store_field(data, 1, CIdx_ocaml);
    Store_field(ocaml_result, 0, data);
    CAMLreturn(ocaml_result);
  }
  else {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 1);
    data = Val_cxerrorcode(result);
    Store_field(ocaml_result, 0, data);
    
    CAMLreturn(ocaml_result);
  }}

CAMLprim value
clang_parseTranslationUnit2FullArgv_wrapper(value CIdx_ocaml, value source_filename_ocaml, value command_line_args_ocaml, value unsaved_files_ocaml, value options_ocaml)
{
  CAMLparam5(CIdx_ocaml, source_filename_ocaml, command_line_args_ocaml, unsaved_files_ocaml, options_ocaml);
  CXIndex CIdx;
  CIdx = Cxindex_val(CIdx_ocaml);
  const char * source_filename;
  source_filename = String_val(source_filename_ocaml);
  int num_command_line_args = Wosize_val(command_line_args_ocaml);
   char * * command_line_args = xmalloc(num_command_line_args * sizeof(const char *const));
  int i; for (i = 0; i < num_command_line_args; i++) {
    command_line_args[i] = String_val(Field(command_line_args_ocaml, i));
  }
  unsigned int num_unsaved_files = Wosize_val(unsaved_files_ocaml);
  struct CXUnsavedFile * unsaved_files = xmalloc(num_unsaved_files * sizeof(struct CXUnsavedFile));
  unsigned int i2; for (i2 = 0; i2 < num_unsaved_files; i2++) {
    unsaved_files[i2] = Cxunsavedfile_val(Field(unsaved_files_ocaml, i2));
  }
  unsigned int options;
  options = Int_val(options_ocaml);
  CXTranslationUnit out_TU;
  enum CXErrorCode result = clang_parseTranslationUnit2FullArgv(CIdx, source_filename, (const char *const *) command_line_args, num_command_line_args, (struct CXUnsavedFile *) unsaved_files, num_unsaved_files, options, &out_TU);
  if (result == CXError_Success) {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 0);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxtranslationunit(out_TU));
  Store_field(data, 1, CIdx_ocaml);
    Store_field(ocaml_result, 0, data);
    CAMLreturn(ocaml_result);
  }
  else {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 1);
    data = Val_cxerrorcode(result);
    Store_field(ocaml_result, 0, data);
    
    CAMLreturn(ocaml_result);
  }}

CAMLprim value
clang_defaultSaveOptions_wrapper(value TU_ocaml)
{
  CAMLparam1(TU_ocaml);
  CXTranslationUnit TU;
  TU = Cxtranslationunit_val(Field(TU_ocaml, 0));
  unsigned int result = clang_defaultSaveOptions(TU);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

enum CXSaveError
Cxsaveerror_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return CXSaveError_Unknown;
  case 1: return CXSaveError_TranslationErrors;
  case 2: return CXSaveError_InvalidTU;
  }
  failwith_fmt("invalid value for Cxsaveerror_val: %d", Int_val(ocaml));
  return CXSaveError_Unknown;
}

value
Val_cxsaveerror(enum CXSaveError v)
{
  switch (v) {
  case CXSaveError_Unknown: return Val_int(0);
  case CXSaveError_TranslationErrors: return Val_int(1);
  case CXSaveError_InvalidTU: return Val_int(2);
  case CXSaveError_None: failwith("unexpected success value");
  }
  failwith_fmt("invalid value for Val_cxsaveerror: %d", v);
  return Val_int(0);
}

CAMLprim value
clang_saveTranslationUnit_wrapper(value TU_ocaml, value FileName_ocaml, value options_ocaml)
{
  CAMLparam3(TU_ocaml, FileName_ocaml, options_ocaml);
  CXTranslationUnit TU;
  TU = Cxtranslationunit_val(Field(TU_ocaml, 0));
  const char * FileName;
  FileName = String_val(FileName_ocaml);
  unsigned int options;
  options = Int_val(options_ocaml);
  int result = clang_saveTranslationUnit(TU, FileName, options);
  if (result == CXSaveError_None) {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 0);
    data = Val_unit;
    Store_field(ocaml_result, 0, data);
    CAMLreturn(ocaml_result);
  }
  else {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 1);
    data = Val_cxsaveerror(result);
    Store_field(ocaml_result, 0, data);
    
    CAMLreturn(ocaml_result);
  }}

CAMLprim value
clang_suspendTranslationUnit_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXTranslationUnit arg;
  arg = Cxtranslationunit_val(Field(arg_ocaml, 0));
  unsigned int result = clang_suspendTranslationUnit(arg);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_defaultReparseOptions_wrapper(value TU_ocaml)
{
  CAMLparam1(TU_ocaml);
  CXTranslationUnit TU;
  TU = Cxtranslationunit_val(Field(TU_ocaml, 0));
  unsigned int result = clang_defaultReparseOptions(TU);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_reparseTranslationUnit_wrapper(value TU_ocaml, value unsaved_files_ocaml, value options_ocaml)
{
  CAMLparam3(TU_ocaml, unsaved_files_ocaml, options_ocaml);
  CXTranslationUnit TU;
  TU = Cxtranslationunit_val(Field(TU_ocaml, 0));
  unsigned int num_unsaved_files = Wosize_val(unsaved_files_ocaml);
  struct CXUnsavedFile * unsaved_files = xmalloc(num_unsaved_files * sizeof(struct CXUnsavedFile));
  unsigned int i; for (i = 0; i < num_unsaved_files; i++) {
    unsaved_files[i] = Cxunsavedfile_val(Field(unsaved_files_ocaml, i));
  }
  unsigned int options;
  options = Int_val(options_ocaml);
  int result = clang_reparseTranslationUnit(TU, num_unsaved_files, (struct CXUnsavedFile *) unsaved_files, options);
  if (result == CXError_Success) {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 0);
    data = Val_unit;
    Store_field(ocaml_result, 0, data);
    CAMLreturn(ocaml_result);
  }
  else {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 1);
    data = Val_cxerrorcode(result);
    Store_field(ocaml_result, 0, data);
    
    CAMLreturn(ocaml_result);
  }}

enum CXTUResourceUsageKind
Cxturesourceusagekind_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return CXTUResourceUsage_AST;
  case 1: return CXTUResourceUsage_Identifiers;
  case 2: return CXTUResourceUsage_Selectors;
  case 3: return CXTUResourceUsage_GlobalCompletionResults;
  case 4: return CXTUResourceUsage_SourceManagerContentCache;
  case 5: return CXTUResourceUsage_AST_SideTables;
  case 6: return CXTUResourceUsage_SourceManager_Membuffer_Malloc;
  case 7: return CXTUResourceUsage_SourceManager_Membuffer_MMap;
  case 8: return CXTUResourceUsage_ExternalASTSource_Membuffer_Malloc;
  case 9: return CXTUResourceUsage_ExternalASTSource_Membuffer_MMap;
  case 10: return CXTUResourceUsage_Preprocessor;
  case 11: return CXTUResourceUsage_PreprocessingRecord;
  case 12: return CXTUResourceUsage_SourceManager_DataStructures;
  case 13: return CXTUResourceUsage_Preprocessor_HeaderSearch;
  }
  failwith_fmt("invalid value for Cxturesourceusagekind_val: %d", Int_val(ocaml));
  return CXTUResourceUsage_AST;
}

value
Val_cxturesourceusagekind(enum CXTUResourceUsageKind v)
{
  switch (v) {
  case CXTUResourceUsage_AST: return Val_int(0);
  case CXTUResourceUsage_Identifiers: return Val_int(1);
  case CXTUResourceUsage_Selectors: return Val_int(2);
  case CXTUResourceUsage_GlobalCompletionResults: return Val_int(3);
  case CXTUResourceUsage_SourceManagerContentCache: return Val_int(4);
  case CXTUResourceUsage_AST_SideTables: return Val_int(5);
  case CXTUResourceUsage_SourceManager_Membuffer_Malloc: return Val_int(6);
  case CXTUResourceUsage_SourceManager_Membuffer_MMap: return Val_int(7);
  case CXTUResourceUsage_ExternalASTSource_Membuffer_Malloc: return Val_int(8);
  case CXTUResourceUsage_ExternalASTSource_Membuffer_MMap: return Val_int(9);
  case CXTUResourceUsage_Preprocessor: return Val_int(10);
  case CXTUResourceUsage_PreprocessingRecord: return Val_int(11);
  case CXTUResourceUsage_SourceManager_DataStructures: return Val_int(12);
  case CXTUResourceUsage_Preprocessor_HeaderSearch: return Val_int(13);
  }
  failwith_fmt("invalid value for Val_cxturesourceusagekind: %d", v);
  return Val_int(0);
}

CAMLprim value
clang_getTUResourceUsageName_wrapper(value kind_ocaml)
{
  CAMLparam1(kind_ocaml);
  enum CXTUResourceUsageKind kind;
  kind = Cxturesourceusagekind_val(kind_ocaml);
  const char * result = clang_getTUResourceUsageName(kind);
  {
    CAMLlocal1(data);
    data = caml_copy_string(result);
    CAMLreturn(data);
  }
}

DECLARE_OPAQUE(struct CXTUResourceUsage, cxturesourceusage, Cxturesourceusage_val, Val_cxturesourceusage, custom_finalize_default)

CAMLprim value
clang_getCXTUResourceUsage_wrapper(value TU_ocaml)
{
  CAMLparam1(TU_ocaml);
  CXTranslationUnit TU;
  TU = Cxtranslationunit_val(Field(TU_ocaml, 0));
  CXTUResourceUsage result = clang_getCXTUResourceUsage(TU);
  {
    CAMLlocal1(data);
    data = Val_cxturesourceusage(result);
    CAMLreturn(data);
  }
}

DECLARE_OPAQUE(CXTargetInfo, cxtargetinfo, Cxtargetinfo_val, Val_cxtargetinfo, custom_finalize_default)

CAMLprim value
clang_getTranslationUnitTargetInfo_wrapper(value CTUnit_ocaml)
{
  CAMLparam1(CTUnit_ocaml);
  CXTranslationUnit CTUnit;
  CTUnit = Cxtranslationunit_val(Field(CTUnit_ocaml, 0));
  CXTargetInfo result = clang_getTranslationUnitTargetInfo(CTUnit);
  {
    CAMLlocal1(data);
    data = Val_cxtargetinfo(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_TargetInfo_getTriple_wrapper(value Info_ocaml)
{
  CAMLparam1(Info_ocaml);
  CXTargetInfo Info;
  Info = Cxtargetinfo_val(Info_ocaml);
  CXString result = clang_TargetInfo_getTriple(Info);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_TargetInfo_getPointerWidth_wrapper(value Info_ocaml)
{
  CAMLparam1(Info_ocaml);
  CXTargetInfo Info;
  Info = Cxtargetinfo_val(Info_ocaml);
  int result = clang_TargetInfo_getPointerWidth(Info);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

enum CXCursorKind
Cxcursorkind_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return CXCursor_UnexposedDecl;
  case 1: return CXCursor_StructDecl;
  case 2: return CXCursor_UnionDecl;
  case 3: return CXCursor_ClassDecl;
  case 4: return CXCursor_EnumDecl;
  case 5: return CXCursor_FieldDecl;
  case 6: return CXCursor_EnumConstantDecl;
  case 7: return CXCursor_FunctionDecl;
  case 8: return CXCursor_VarDecl;
  case 9: return CXCursor_ParmDecl;
  case 10: return CXCursor_ObjCInterfaceDecl;
  case 11: return CXCursor_ObjCCategoryDecl;
  case 12: return CXCursor_ObjCProtocolDecl;
  case 13: return CXCursor_ObjCPropertyDecl;
  case 14: return CXCursor_ObjCIvarDecl;
  case 15: return CXCursor_ObjCInstanceMethodDecl;
  case 16: return CXCursor_ObjCClassMethodDecl;
  case 17: return CXCursor_ObjCImplementationDecl;
  case 18: return CXCursor_ObjCCategoryImplDecl;
  case 19: return CXCursor_TypedefDecl;
  case 20: return CXCursor_CXXMethod;
  case 21: return CXCursor_Namespace;
  case 22: return CXCursor_LinkageSpec;
  case 23: return CXCursor_Constructor;
  case 24: return CXCursor_Destructor;
  case 25: return CXCursor_ConversionFunction;
  case 26: return CXCursor_TemplateTypeParameter;
  case 27: return CXCursor_NonTypeTemplateParameter;
  case 28: return CXCursor_TemplateTemplateParameter;
  case 29: return CXCursor_FunctionTemplate;
  case 30: return CXCursor_ClassTemplate;
  case 31: return CXCursor_ClassTemplatePartialSpecialization;
  case 32: return CXCursor_NamespaceAlias;
  case 33: return CXCursor_UsingDirective;
  case 34: return CXCursor_UsingDeclaration;
  case 35: return CXCursor_TypeAliasDecl;
  case 36: return CXCursor_ObjCSynthesizeDecl;
  case 37: return CXCursor_ObjCDynamicDecl;
  case 38: return CXCursor_CXXAccessSpecifier;
  case 39: return CXCursor_ObjCSuperClassRef;
  case 40: return CXCursor_ObjCProtocolRef;
  case 41: return CXCursor_ObjCClassRef;
  case 42: return CXCursor_TypeRef;
  case 43: return CXCursor_CXXBaseSpecifier;
  case 44: return CXCursor_TemplateRef;
  case 45: return CXCursor_NamespaceRef;
  case 46: return CXCursor_MemberRef;
  case 47: return CXCursor_LabelRef;
  case 48: return CXCursor_OverloadedDeclRef;
  case 49: return CXCursor_VariableRef;
  case 50: return CXCursor_InvalidFile;
  case 51: return CXCursor_NoDeclFound;
  case 52: return CXCursor_NotImplemented;
  case 53: return CXCursor_InvalidCode;
  case 54: return CXCursor_UnexposedExpr;
  case 55: return CXCursor_DeclRefExpr;
  case 56: return CXCursor_MemberRefExpr;
  case 57: return CXCursor_CallExpr;
  case 58: return CXCursor_ObjCMessageExpr;
  case 59: return CXCursor_BlockExpr;
  case 60: return CXCursor_IntegerLiteral;
  case 61: return CXCursor_FloatingLiteral;
  case 62: return CXCursor_ImaginaryLiteral;
  case 63: return CXCursor_StringLiteral;
  case 64: return CXCursor_CharacterLiteral;
  case 65: return CXCursor_ParenExpr;
  case 66: return CXCursor_UnaryOperator;
  case 67: return CXCursor_ArraySubscriptExpr;
  case 68: return CXCursor_BinaryOperator;
  case 69: return CXCursor_CompoundAssignOperator;
  case 70: return CXCursor_ConditionalOperator;
  case 71: return CXCursor_CStyleCastExpr;
  case 72: return CXCursor_CompoundLiteralExpr;
  case 73: return CXCursor_InitListExpr;
  case 74: return CXCursor_AddrLabelExpr;
  case 75: return CXCursor_StmtExpr;
  case 76: return CXCursor_GenericSelectionExpr;
  case 77: return CXCursor_GNUNullExpr;
  case 78: return CXCursor_CXXStaticCastExpr;
  case 79: return CXCursor_CXXDynamicCastExpr;
  case 80: return CXCursor_CXXReinterpretCastExpr;
  case 81: return CXCursor_CXXConstCastExpr;
  case 82: return CXCursor_CXXFunctionalCastExpr;
  case 83: return CXCursor_CXXTypeidExpr;
  case 84: return CXCursor_CXXBoolLiteralExpr;
  case 85: return CXCursor_CXXNullPtrLiteralExpr;
  case 86: return CXCursor_CXXThisExpr;
  case 87: return CXCursor_CXXThrowExpr;
  case 88: return CXCursor_CXXNewExpr;
  case 89: return CXCursor_CXXDeleteExpr;
  case 90: return CXCursor_UnaryExpr;
  case 91: return CXCursor_ObjCStringLiteral;
  case 92: return CXCursor_ObjCEncodeExpr;
  case 93: return CXCursor_ObjCSelectorExpr;
  case 94: return CXCursor_ObjCProtocolExpr;
  case 95: return CXCursor_ObjCBridgedCastExpr;
  case 96: return CXCursor_PackExpansionExpr;
  case 97: return CXCursor_SizeOfPackExpr;
  case 98: return CXCursor_LambdaExpr;
  case 99: return CXCursor_ObjCBoolLiteralExpr;
  case 100: return CXCursor_ObjCSelfExpr;
  case 101: return CXCursor_OMPArraySectionExpr;
  case 102: return CXCursor_ObjCAvailabilityCheckExpr;
  case 103: return CXCursor_FixedPointLiteral;
  case 104: return CXCursor_UnexposedStmt;
  case 105: return CXCursor_LabelStmt;
  case 106: return CXCursor_CompoundStmt;
  case 107: return CXCursor_CaseStmt;
  case 108: return CXCursor_DefaultStmt;
  case 109: return CXCursor_IfStmt;
  case 110: return CXCursor_SwitchStmt;
  case 111: return CXCursor_WhileStmt;
  case 112: return CXCursor_DoStmt;
  case 113: return CXCursor_ForStmt;
  case 114: return CXCursor_GotoStmt;
  case 115: return CXCursor_IndirectGotoStmt;
  case 116: return CXCursor_ContinueStmt;
  case 117: return CXCursor_BreakStmt;
  case 118: return CXCursor_ReturnStmt;
  case 119: return CXCursor_GCCAsmStmt;
  case 120: return CXCursor_ObjCAtTryStmt;
  case 121: return CXCursor_ObjCAtCatchStmt;
  case 122: return CXCursor_ObjCAtFinallyStmt;
  case 123: return CXCursor_ObjCAtThrowStmt;
  case 124: return CXCursor_ObjCAtSynchronizedStmt;
  case 125: return CXCursor_ObjCAutoreleasePoolStmt;
  case 126: return CXCursor_ObjCForCollectionStmt;
  case 127: return CXCursor_CXXCatchStmt;
  case 128: return CXCursor_CXXTryStmt;
  case 129: return CXCursor_CXXForRangeStmt;
  case 130: return CXCursor_SEHTryStmt;
  case 131: return CXCursor_SEHExceptStmt;
  case 132: return CXCursor_SEHFinallyStmt;
  case 133: return CXCursor_MSAsmStmt;
  case 134: return CXCursor_NullStmt;
  case 135: return CXCursor_DeclStmt;
  case 136: return CXCursor_OMPParallelDirective;
  case 137: return CXCursor_OMPSimdDirective;
  case 138: return CXCursor_OMPForDirective;
  case 139: return CXCursor_OMPSectionsDirective;
  case 140: return CXCursor_OMPSectionDirective;
  case 141: return CXCursor_OMPSingleDirective;
  case 142: return CXCursor_OMPParallelForDirective;
  case 143: return CXCursor_OMPParallelSectionsDirective;
  case 144: return CXCursor_OMPTaskDirective;
  case 145: return CXCursor_OMPMasterDirective;
  case 146: return CXCursor_OMPCriticalDirective;
  case 147: return CXCursor_OMPTaskyieldDirective;
  case 148: return CXCursor_OMPBarrierDirective;
  case 149: return CXCursor_OMPTaskwaitDirective;
  case 150: return CXCursor_OMPFlushDirective;
  case 151: return CXCursor_SEHLeaveStmt;
  case 152: return CXCursor_OMPOrderedDirective;
  case 153: return CXCursor_OMPAtomicDirective;
  case 154: return CXCursor_OMPForSimdDirective;
  case 155: return CXCursor_OMPParallelForSimdDirective;
  case 156: return CXCursor_OMPTargetDirective;
  case 157: return CXCursor_OMPTeamsDirective;
  case 158: return CXCursor_OMPTaskgroupDirective;
  case 159: return CXCursor_OMPCancellationPointDirective;
  case 160: return CXCursor_OMPCancelDirective;
  case 161: return CXCursor_OMPTargetDataDirective;
  case 162: return CXCursor_OMPTaskLoopDirective;
  case 163: return CXCursor_OMPTaskLoopSimdDirective;
  case 164: return CXCursor_OMPDistributeDirective;
  case 165: return CXCursor_OMPTargetEnterDataDirective;
  case 166: return CXCursor_OMPTargetExitDataDirective;
  case 167: return CXCursor_OMPTargetParallelDirective;
  case 168: return CXCursor_OMPTargetParallelForDirective;
  case 169: return CXCursor_OMPTargetUpdateDirective;
  case 170: return CXCursor_OMPDistributeParallelForDirective;
  case 171: return CXCursor_OMPDistributeParallelForSimdDirective;
  case 172: return CXCursor_OMPDistributeSimdDirective;
  case 173: return CXCursor_OMPTargetParallelForSimdDirective;
  case 174: return CXCursor_OMPTargetSimdDirective;
  case 175: return CXCursor_OMPTeamsDistributeDirective;
  case 176: return CXCursor_OMPTeamsDistributeSimdDirective;
  case 177: return CXCursor_OMPTeamsDistributeParallelForSimdDirective;
  case 178: return CXCursor_OMPTeamsDistributeParallelForDirective;
  case 179: return CXCursor_OMPTargetTeamsDirective;
  case 180: return CXCursor_OMPTargetTeamsDistributeDirective;
  case 181: return CXCursor_OMPTargetTeamsDistributeParallelForDirective;
  case 182: return CXCursor_OMPTargetTeamsDistributeParallelForSimdDirective;
  case 183: return CXCursor_OMPTargetTeamsDistributeSimdDirective;
  case 184: return CXCursor_TranslationUnit;
  case 185: return CXCursor_UnexposedAttr;
  case 186: return CXCursor_IBActionAttr;
  case 187: return CXCursor_IBOutletAttr;
  case 188: return CXCursor_IBOutletCollectionAttr;
  case 189: return CXCursor_CXXFinalAttr;
  case 190: return CXCursor_CXXOverrideAttr;
  case 191: return CXCursor_AnnotateAttr;
  case 192: return CXCursor_AsmLabelAttr;
  case 193: return CXCursor_PackedAttr;
  case 194: return CXCursor_PureAttr;
  case 195: return CXCursor_ConstAttr;
  case 196: return CXCursor_NoDuplicateAttr;
  case 197: return CXCursor_CUDAConstantAttr;
  case 198: return CXCursor_CUDADeviceAttr;
  case 199: return CXCursor_CUDAGlobalAttr;
  case 200: return CXCursor_CUDAHostAttr;
  case 201: return CXCursor_CUDASharedAttr;
  case 202: return CXCursor_VisibilityAttr;
  case 203: return CXCursor_DLLExport;
  case 204: return CXCursor_DLLImport;
  case 205: return CXCursor_PreprocessingDirective;
  case 206: return CXCursor_MacroDefinition;
  case 207: return CXCursor_MacroExpansion;
  case 208: return CXCursor_InclusionDirective;
  case 209: return CXCursor_ModuleImportDecl;
  case 210: return CXCursor_TypeAliasTemplateDecl;
  case 211: return CXCursor_StaticAssert;
  case 212: return CXCursor_FriendDecl;
  case 213: return CXCursor_OverloadCandidate;
  }
  failwith_fmt("invalid value for Cxcursorkind_val: %d", Int_val(ocaml));
  return CXCursor_UnexposedDecl;
}

value
Val_cxcursorkind(enum CXCursorKind v)
{
  switch (v) {
  case CXCursor_UnexposedDecl: return Val_int(0);
  case CXCursor_StructDecl: return Val_int(1);
  case CXCursor_UnionDecl: return Val_int(2);
  case CXCursor_ClassDecl: return Val_int(3);
  case CXCursor_EnumDecl: return Val_int(4);
  case CXCursor_FieldDecl: return Val_int(5);
  case CXCursor_EnumConstantDecl: return Val_int(6);
  case CXCursor_FunctionDecl: return Val_int(7);
  case CXCursor_VarDecl: return Val_int(8);
  case CXCursor_ParmDecl: return Val_int(9);
  case CXCursor_ObjCInterfaceDecl: return Val_int(10);
  case CXCursor_ObjCCategoryDecl: return Val_int(11);
  case CXCursor_ObjCProtocolDecl: return Val_int(12);
  case CXCursor_ObjCPropertyDecl: return Val_int(13);
  case CXCursor_ObjCIvarDecl: return Val_int(14);
  case CXCursor_ObjCInstanceMethodDecl: return Val_int(15);
  case CXCursor_ObjCClassMethodDecl: return Val_int(16);
  case CXCursor_ObjCImplementationDecl: return Val_int(17);
  case CXCursor_ObjCCategoryImplDecl: return Val_int(18);
  case CXCursor_TypedefDecl: return Val_int(19);
  case CXCursor_CXXMethod: return Val_int(20);
  case CXCursor_Namespace: return Val_int(21);
  case CXCursor_LinkageSpec: return Val_int(22);
  case CXCursor_Constructor: return Val_int(23);
  case CXCursor_Destructor: return Val_int(24);
  case CXCursor_ConversionFunction: return Val_int(25);
  case CXCursor_TemplateTypeParameter: return Val_int(26);
  case CXCursor_NonTypeTemplateParameter: return Val_int(27);
  case CXCursor_TemplateTemplateParameter: return Val_int(28);
  case CXCursor_FunctionTemplate: return Val_int(29);
  case CXCursor_ClassTemplate: return Val_int(30);
  case CXCursor_ClassTemplatePartialSpecialization: return Val_int(31);
  case CXCursor_NamespaceAlias: return Val_int(32);
  case CXCursor_UsingDirective: return Val_int(33);
  case CXCursor_UsingDeclaration: return Val_int(34);
  case CXCursor_TypeAliasDecl: return Val_int(35);
  case CXCursor_ObjCSynthesizeDecl: return Val_int(36);
  case CXCursor_ObjCDynamicDecl: return Val_int(37);
  case CXCursor_CXXAccessSpecifier: return Val_int(38);
  case CXCursor_ObjCSuperClassRef: return Val_int(39);
  case CXCursor_ObjCProtocolRef: return Val_int(40);
  case CXCursor_ObjCClassRef: return Val_int(41);
  case CXCursor_TypeRef: return Val_int(42);
  case CXCursor_CXXBaseSpecifier: return Val_int(43);
  case CXCursor_TemplateRef: return Val_int(44);
  case CXCursor_NamespaceRef: return Val_int(45);
  case CXCursor_MemberRef: return Val_int(46);
  case CXCursor_LabelRef: return Val_int(47);
  case CXCursor_OverloadedDeclRef: return Val_int(48);
  case CXCursor_VariableRef: return Val_int(49);
  case CXCursor_InvalidFile: return Val_int(50);
  case CXCursor_NoDeclFound: return Val_int(51);
  case CXCursor_NotImplemented: return Val_int(52);
  case CXCursor_InvalidCode: return Val_int(53);
  case CXCursor_UnexposedExpr: return Val_int(54);
  case CXCursor_DeclRefExpr: return Val_int(55);
  case CXCursor_MemberRefExpr: return Val_int(56);
  case CXCursor_CallExpr: return Val_int(57);
  case CXCursor_ObjCMessageExpr: return Val_int(58);
  case CXCursor_BlockExpr: return Val_int(59);
  case CXCursor_IntegerLiteral: return Val_int(60);
  case CXCursor_FloatingLiteral: return Val_int(61);
  case CXCursor_ImaginaryLiteral: return Val_int(62);
  case CXCursor_StringLiteral: return Val_int(63);
  case CXCursor_CharacterLiteral: return Val_int(64);
  case CXCursor_ParenExpr: return Val_int(65);
  case CXCursor_UnaryOperator: return Val_int(66);
  case CXCursor_ArraySubscriptExpr: return Val_int(67);
  case CXCursor_BinaryOperator: return Val_int(68);
  case CXCursor_CompoundAssignOperator: return Val_int(69);
  case CXCursor_ConditionalOperator: return Val_int(70);
  case CXCursor_CStyleCastExpr: return Val_int(71);
  case CXCursor_CompoundLiteralExpr: return Val_int(72);
  case CXCursor_InitListExpr: return Val_int(73);
  case CXCursor_AddrLabelExpr: return Val_int(74);
  case CXCursor_StmtExpr: return Val_int(75);
  case CXCursor_GenericSelectionExpr: return Val_int(76);
  case CXCursor_GNUNullExpr: return Val_int(77);
  case CXCursor_CXXStaticCastExpr: return Val_int(78);
  case CXCursor_CXXDynamicCastExpr: return Val_int(79);
  case CXCursor_CXXReinterpretCastExpr: return Val_int(80);
  case CXCursor_CXXConstCastExpr: return Val_int(81);
  case CXCursor_CXXFunctionalCastExpr: return Val_int(82);
  case CXCursor_CXXTypeidExpr: return Val_int(83);
  case CXCursor_CXXBoolLiteralExpr: return Val_int(84);
  case CXCursor_CXXNullPtrLiteralExpr: return Val_int(85);
  case CXCursor_CXXThisExpr: return Val_int(86);
  case CXCursor_CXXThrowExpr: return Val_int(87);
  case CXCursor_CXXNewExpr: return Val_int(88);
  case CXCursor_CXXDeleteExpr: return Val_int(89);
  case CXCursor_UnaryExpr: return Val_int(90);
  case CXCursor_ObjCStringLiteral: return Val_int(91);
  case CXCursor_ObjCEncodeExpr: return Val_int(92);
  case CXCursor_ObjCSelectorExpr: return Val_int(93);
  case CXCursor_ObjCProtocolExpr: return Val_int(94);
  case CXCursor_ObjCBridgedCastExpr: return Val_int(95);
  case CXCursor_PackExpansionExpr: return Val_int(96);
  case CXCursor_SizeOfPackExpr: return Val_int(97);
  case CXCursor_LambdaExpr: return Val_int(98);
  case CXCursor_ObjCBoolLiteralExpr: return Val_int(99);
  case CXCursor_ObjCSelfExpr: return Val_int(100);
  case CXCursor_OMPArraySectionExpr: return Val_int(101);
  case CXCursor_ObjCAvailabilityCheckExpr: return Val_int(102);
  case CXCursor_FixedPointLiteral: return Val_int(103);
  case CXCursor_UnexposedStmt: return Val_int(104);
  case CXCursor_LabelStmt: return Val_int(105);
  case CXCursor_CompoundStmt: return Val_int(106);
  case CXCursor_CaseStmt: return Val_int(107);
  case CXCursor_DefaultStmt: return Val_int(108);
  case CXCursor_IfStmt: return Val_int(109);
  case CXCursor_SwitchStmt: return Val_int(110);
  case CXCursor_WhileStmt: return Val_int(111);
  case CXCursor_DoStmt: return Val_int(112);
  case CXCursor_ForStmt: return Val_int(113);
  case CXCursor_GotoStmt: return Val_int(114);
  case CXCursor_IndirectGotoStmt: return Val_int(115);
  case CXCursor_ContinueStmt: return Val_int(116);
  case CXCursor_BreakStmt: return Val_int(117);
  case CXCursor_ReturnStmt: return Val_int(118);
  case CXCursor_GCCAsmStmt: return Val_int(119);
  case CXCursor_ObjCAtTryStmt: return Val_int(120);
  case CXCursor_ObjCAtCatchStmt: return Val_int(121);
  case CXCursor_ObjCAtFinallyStmt: return Val_int(122);
  case CXCursor_ObjCAtThrowStmt: return Val_int(123);
  case CXCursor_ObjCAtSynchronizedStmt: return Val_int(124);
  case CXCursor_ObjCAutoreleasePoolStmt: return Val_int(125);
  case CXCursor_ObjCForCollectionStmt: return Val_int(126);
  case CXCursor_CXXCatchStmt: return Val_int(127);
  case CXCursor_CXXTryStmt: return Val_int(128);
  case CXCursor_CXXForRangeStmt: return Val_int(129);
  case CXCursor_SEHTryStmt: return Val_int(130);
  case CXCursor_SEHExceptStmt: return Val_int(131);
  case CXCursor_SEHFinallyStmt: return Val_int(132);
  case CXCursor_MSAsmStmt: return Val_int(133);
  case CXCursor_NullStmt: return Val_int(134);
  case CXCursor_DeclStmt: return Val_int(135);
  case CXCursor_OMPParallelDirective: return Val_int(136);
  case CXCursor_OMPSimdDirective: return Val_int(137);
  case CXCursor_OMPForDirective: return Val_int(138);
  case CXCursor_OMPSectionsDirective: return Val_int(139);
  case CXCursor_OMPSectionDirective: return Val_int(140);
  case CXCursor_OMPSingleDirective: return Val_int(141);
  case CXCursor_OMPParallelForDirective: return Val_int(142);
  case CXCursor_OMPParallelSectionsDirective: return Val_int(143);
  case CXCursor_OMPTaskDirective: return Val_int(144);
  case CXCursor_OMPMasterDirective: return Val_int(145);
  case CXCursor_OMPCriticalDirective: return Val_int(146);
  case CXCursor_OMPTaskyieldDirective: return Val_int(147);
  case CXCursor_OMPBarrierDirective: return Val_int(148);
  case CXCursor_OMPTaskwaitDirective: return Val_int(149);
  case CXCursor_OMPFlushDirective: return Val_int(150);
  case CXCursor_SEHLeaveStmt: return Val_int(151);
  case CXCursor_OMPOrderedDirective: return Val_int(152);
  case CXCursor_OMPAtomicDirective: return Val_int(153);
  case CXCursor_OMPForSimdDirective: return Val_int(154);
  case CXCursor_OMPParallelForSimdDirective: return Val_int(155);
  case CXCursor_OMPTargetDirective: return Val_int(156);
  case CXCursor_OMPTeamsDirective: return Val_int(157);
  case CXCursor_OMPTaskgroupDirective: return Val_int(158);
  case CXCursor_OMPCancellationPointDirective: return Val_int(159);
  case CXCursor_OMPCancelDirective: return Val_int(160);
  case CXCursor_OMPTargetDataDirective: return Val_int(161);
  case CXCursor_OMPTaskLoopDirective: return Val_int(162);
  case CXCursor_OMPTaskLoopSimdDirective: return Val_int(163);
  case CXCursor_OMPDistributeDirective: return Val_int(164);
  case CXCursor_OMPTargetEnterDataDirective: return Val_int(165);
  case CXCursor_OMPTargetExitDataDirective: return Val_int(166);
  case CXCursor_OMPTargetParallelDirective: return Val_int(167);
  case CXCursor_OMPTargetParallelForDirective: return Val_int(168);
  case CXCursor_OMPTargetUpdateDirective: return Val_int(169);
  case CXCursor_OMPDistributeParallelForDirective: return Val_int(170);
  case CXCursor_OMPDistributeParallelForSimdDirective: return Val_int(171);
  case CXCursor_OMPDistributeSimdDirective: return Val_int(172);
  case CXCursor_OMPTargetParallelForSimdDirective: return Val_int(173);
  case CXCursor_OMPTargetSimdDirective: return Val_int(174);
  case CXCursor_OMPTeamsDistributeDirective: return Val_int(175);
  case CXCursor_OMPTeamsDistributeSimdDirective: return Val_int(176);
  case CXCursor_OMPTeamsDistributeParallelForSimdDirective: return Val_int(177);
  case CXCursor_OMPTeamsDistributeParallelForDirective: return Val_int(178);
  case CXCursor_OMPTargetTeamsDirective: return Val_int(179);
  case CXCursor_OMPTargetTeamsDistributeDirective: return Val_int(180);
  case CXCursor_OMPTargetTeamsDistributeParallelForDirective: return Val_int(181);
  case CXCursor_OMPTargetTeamsDistributeParallelForSimdDirective: return Val_int(182);
  case CXCursor_OMPTargetTeamsDistributeSimdDirective: return Val_int(183);
  case CXCursor_TranslationUnit: return Val_int(184);
  case CXCursor_UnexposedAttr: return Val_int(185);
  case CXCursor_IBActionAttr: return Val_int(186);
  case CXCursor_IBOutletAttr: return Val_int(187);
  case CXCursor_IBOutletCollectionAttr: return Val_int(188);
  case CXCursor_CXXFinalAttr: return Val_int(189);
  case CXCursor_CXXOverrideAttr: return Val_int(190);
  case CXCursor_AnnotateAttr: return Val_int(191);
  case CXCursor_AsmLabelAttr: return Val_int(192);
  case CXCursor_PackedAttr: return Val_int(193);
  case CXCursor_PureAttr: return Val_int(194);
  case CXCursor_ConstAttr: return Val_int(195);
  case CXCursor_NoDuplicateAttr: return Val_int(196);
  case CXCursor_CUDAConstantAttr: return Val_int(197);
  case CXCursor_CUDADeviceAttr: return Val_int(198);
  case CXCursor_CUDAGlobalAttr: return Val_int(199);
  case CXCursor_CUDAHostAttr: return Val_int(200);
  case CXCursor_CUDASharedAttr: return Val_int(201);
  case CXCursor_VisibilityAttr: return Val_int(202);
  case CXCursor_DLLExport: return Val_int(203);
  case CXCursor_DLLImport: return Val_int(204);
  case CXCursor_PreprocessingDirective: return Val_int(205);
  case CXCursor_MacroDefinition: return Val_int(206);
  case CXCursor_MacroExpansion: return Val_int(207);
  case CXCursor_InclusionDirective: return Val_int(208);
  case CXCursor_ModuleImportDecl: return Val_int(209);
  case CXCursor_TypeAliasTemplateDecl: return Val_int(210);
  case CXCursor_StaticAssert: return Val_int(211);
  case CXCursor_FriendDecl: return Val_int(212);
  case CXCursor_OverloadCandidate: return Val_int(213);
  }
  failwith_fmt("invalid value for Val_cxcursorkind: %d", v);
  return Val_int(0);
}

DECLARE_OPAQUE(CXCursor, cxcursor, Cxcursor_val, Val_cxcursor, custom_finalize_default)

CAMLprim value
clang_getNullCursor_wrapper()
{
  CAMLparam0();
  CXCursor result = clang_getNullCursor();
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(1);
  Store_field(data, 0, Val_cxcursor(result));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getTranslationUnitCursor_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXTranslationUnit arg;
  arg = Cxtranslationunit_val(Field(arg_ocaml, 0));
  CXCursor result = clang_getTranslationUnitCursor(arg);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxcursor(result));
  Store_field(data, 1, arg_ocaml);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_equalCursors_wrapper(value arg_ocaml, value arg2_ocaml)
{
  CAMLparam2(arg_ocaml, arg2_ocaml);
  CXCursor arg;
  arg = Cxcursor_val(Field(arg_ocaml, 0));
  CXCursor arg2;
  arg2 = Cxcursor_val(Field(arg2_ocaml, 0));
  unsigned int result = clang_equalCursors(arg, arg2);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_isNull_wrapper(value cursor_ocaml)
{
  CAMLparam1(cursor_ocaml);
  CXCursor cursor;
  cursor = Cxcursor_val(Field(cursor_ocaml, 0));
  int result = clang_Cursor_isNull(cursor);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_hashCursor_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXCursor arg;
  arg = Cxcursor_val(Field(arg_ocaml, 0));
  unsigned int result = clang_hashCursor(arg);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCursorKind_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXCursor arg;
  arg = Cxcursor_val(Field(arg_ocaml, 0));
  enum CXCursorKind result = clang_getCursorKind(arg);
  {
    CAMLlocal1(data);
    data = Val_cxcursorkind(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_isDeclaration_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  enum CXCursorKind arg;
  arg = Cxcursorkind_val(arg_ocaml);
  unsigned int result = clang_isDeclaration(arg);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_isInvalidDeclaration_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXCursor arg;
  arg = Cxcursor_val(Field(arg_ocaml, 0));
  unsigned int result = clang_isInvalidDeclaration(arg);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_isReference_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  enum CXCursorKind arg;
  arg = Cxcursorkind_val(arg_ocaml);
  unsigned int result = clang_isReference(arg);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_isExpression_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  enum CXCursorKind arg;
  arg = Cxcursorkind_val(arg_ocaml);
  unsigned int result = clang_isExpression(arg);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_isStatement_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  enum CXCursorKind arg;
  arg = Cxcursorkind_val(arg_ocaml);
  unsigned int result = clang_isStatement(arg);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_isAttribute_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  enum CXCursorKind arg;
  arg = Cxcursorkind_val(arg_ocaml);
  unsigned int result = clang_isAttribute(arg);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_hasAttrs_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int result = clang_Cursor_hasAttrs(C);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_isInvalid_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  enum CXCursorKind arg;
  arg = Cxcursorkind_val(arg_ocaml);
  unsigned int result = clang_isInvalid(arg);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_isTranslationUnit_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  enum CXCursorKind arg;
  arg = Cxcursorkind_val(arg_ocaml);
  unsigned int result = clang_isTranslationUnit(arg);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_isPreprocessing_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  enum CXCursorKind arg;
  arg = Cxcursorkind_val(arg_ocaml);
  unsigned int result = clang_isPreprocessing(arg);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_isUnexposed_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  enum CXCursorKind arg;
  arg = Cxcursorkind_val(arg_ocaml);
  unsigned int result = clang_isUnexposed(arg);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

enum CXLinkageKind
Cxlinkagekind_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return CXLinkage_Invalid;
  case 1: return CXLinkage_NoLinkage;
  case 2: return CXLinkage_Internal;
  case 3: return CXLinkage_UniqueExternal;
  case 4: return CXLinkage_External;
  }
  failwith_fmt("invalid value for Cxlinkagekind_val: %d", Int_val(ocaml));
  return CXLinkage_Invalid;
}

value
Val_cxlinkagekind(enum CXLinkageKind v)
{
  switch (v) {
  case CXLinkage_Invalid: return Val_int(0);
  case CXLinkage_NoLinkage: return Val_int(1);
  case CXLinkage_Internal: return Val_int(2);
  case CXLinkage_UniqueExternal: return Val_int(3);
  case CXLinkage_External: return Val_int(4);
  }
  failwith_fmt("invalid value for Val_cxlinkagekind: %d", v);
  return Val_int(0);
}

CAMLprim value
clang_getCursorLinkage_wrapper(value cursor_ocaml)
{
  CAMLparam1(cursor_ocaml);
  CXCursor cursor;
  cursor = Cxcursor_val(Field(cursor_ocaml, 0));
  enum CXLinkageKind result = clang_getCursorLinkage(cursor);
  {
    CAMLlocal1(data);
    data = Val_cxlinkagekind(result);
    CAMLreturn(data);
  }
}

enum CXVisibilityKind
Cxvisibilitykind_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return CXVisibility_Invalid;
  case 1: return CXVisibility_Hidden;
  case 2: return CXVisibility_Protected;
  case 3: return CXVisibility_Default;
  }
  failwith_fmt("invalid value for Cxvisibilitykind_val: %d", Int_val(ocaml));
  return CXVisibility_Invalid;
}

value
Val_cxvisibilitykind(enum CXVisibilityKind v)
{
  switch (v) {
  case CXVisibility_Invalid: return Val_int(0);
  case CXVisibility_Hidden: return Val_int(1);
  case CXVisibility_Protected: return Val_int(2);
  case CXVisibility_Default: return Val_int(3);
  }
  failwith_fmt("invalid value for Val_cxvisibilitykind: %d", v);
  return Val_int(0);
}

CAMLprim value
clang_getCursorVisibility_wrapper(value cursor_ocaml)
{
  CAMLparam1(cursor_ocaml);
  CXCursor cursor;
  cursor = Cxcursor_val(Field(cursor_ocaml, 0));
  enum CXVisibilityKind result = clang_getCursorVisibility(cursor);
  {
    CAMLlocal1(data);
    data = Val_cxvisibilitykind(result);
    CAMLreturn(data);
  }
}

enum CXAvailabilityKind
Cxavailabilitykind_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return CXAvailability_Available;
  case 1: return CXAvailability_Deprecated;
  case 2: return CXAvailability_NotAvailable;
  case 3: return CXAvailability_NotAccessible;
  }
  failwith_fmt("invalid value for Cxavailabilitykind_val: %d", Int_val(ocaml));
  return CXAvailability_Available;
}

value
Val_cxavailabilitykind(enum CXAvailabilityKind v)
{
  switch (v) {
  case CXAvailability_Available: return Val_int(0);
  case CXAvailability_Deprecated: return Val_int(1);
  case CXAvailability_NotAvailable: return Val_int(2);
  case CXAvailability_NotAccessible: return Val_int(3);
  }
  failwith_fmt("invalid value for Val_cxavailabilitykind: %d", v);
  return Val_int(0);
}

CAMLprim value
clang_getCursorAvailability_wrapper(value cursor_ocaml)
{
  CAMLparam1(cursor_ocaml);
  CXCursor cursor;
  cursor = Cxcursor_val(Field(cursor_ocaml, 0));
  enum CXAvailabilityKind result = clang_getCursorAvailability(cursor);
  {
    CAMLlocal1(data);
    data = Val_cxavailabilitykind(result);
    CAMLreturn(data);
  }
}

enum CXLanguageKind
Cxlanguagekind_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return CXLanguage_Invalid;
  case 1: return CXLanguage_C;
  case 2: return CXLanguage_ObjC;
  case 3: return CXLanguage_CPlusPlus;
  }
  failwith_fmt("invalid value for Cxlanguagekind_val: %d", Int_val(ocaml));
  return CXLanguage_Invalid;
}

value
Val_cxlanguagekind(enum CXLanguageKind v)
{
  switch (v) {
  case CXLanguage_Invalid: return Val_int(0);
  case CXLanguage_C: return Val_int(1);
  case CXLanguage_ObjC: return Val_int(2);
  case CXLanguage_CPlusPlus: return Val_int(3);
  }
  failwith_fmt("invalid value for Val_cxlanguagekind: %d", v);
  return Val_int(0);
}

CAMLprim value
clang_getCursorLanguage_wrapper(value cursor_ocaml)
{
  CAMLparam1(cursor_ocaml);
  CXCursor cursor;
  cursor = Cxcursor_val(Field(cursor_ocaml, 0));
  enum CXLanguageKind result = clang_getCursorLanguage(cursor);
  {
    CAMLlocal1(data);
    data = Val_cxlanguagekind(result);
    CAMLreturn(data);
  }
}

enum CXTLSKind
Cxtlskind_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return CXTLS_None;
  case 1: return CXTLS_Dynamic;
  case 2: return CXTLS_Static;
  }
  failwith_fmt("invalid value for Cxtlskind_val: %d", Int_val(ocaml));
  return CXTLS_None;
}

value
Val_cxtlskind(enum CXTLSKind v)
{
  switch (v) {
  case CXTLS_None: return Val_int(0);
  case CXTLS_Dynamic: return Val_int(1);
  case CXTLS_Static: return Val_int(2);
  }
  failwith_fmt("invalid value for Val_cxtlskind: %d", v);
  return Val_int(0);
}

CAMLprim value
clang_getCursorTLSKind_wrapper(value cursor_ocaml)
{
  CAMLparam1(cursor_ocaml);
  CXCursor cursor;
  cursor = Cxcursor_val(Field(cursor_ocaml, 0));
  enum CXTLSKind result = clang_getCursorTLSKind(cursor);
  {
    CAMLlocal1(data);
    data = Val_cxtlskind(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_getTranslationUnit_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXCursor arg;
  arg = Cxcursor_val(Field(arg_ocaml, 0));
  CXTranslationUnit result = clang_Cursor_getTranslationUnit(arg);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxtranslationunit(result));
  Store_field(data, 1, Field(Field(arg_ocaml, 1), 1));
    CAMLreturn(data);
  }
}

DECLARE_OPAQUE(CXCursorSet, cxcursorset, Cxcursorset_val, Val_cxcursorset, custom_finalize_default)

CAMLprim value
clang_createCXCursorSet_wrapper()
{
  CAMLparam0();
  CXCursorSet result = clang_createCXCursorSet();
  {
    CAMLlocal1(data);
    data = Val_cxcursorset(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_CXCursorSet_contains_wrapper(value cset_ocaml, value cursor_ocaml)
{
  CAMLparam2(cset_ocaml, cursor_ocaml);
  CXCursorSet cset;
  cset = Cxcursorset_val(cset_ocaml);
  CXCursor cursor;
  cursor = Cxcursor_val(Field(cursor_ocaml, 0));
  unsigned int result = clang_CXCursorSet_contains(cset, cursor);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_CXCursorSet_insert_wrapper(value cset_ocaml, value cursor_ocaml)
{
  CAMLparam2(cset_ocaml, cursor_ocaml);
  CXCursorSet cset;
  cset = Cxcursorset_val(cset_ocaml);
  CXCursor cursor;
  cursor = Cxcursor_val(Field(cursor_ocaml, 0));
  unsigned int result = clang_CXCursorSet_insert(cset, cursor);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCursorSemanticParent_wrapper(value cursor_ocaml)
{
  CAMLparam1(cursor_ocaml);
  CXCursor cursor;
  cursor = Cxcursor_val(Field(cursor_ocaml, 0));
  CXCursor result = clang_getCursorSemanticParent(cursor);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxcursor(result));
  Store_field(data, 1, Field(cursor_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCursorLexicalParent_wrapper(value cursor_ocaml)
{
  CAMLparam1(cursor_ocaml);
  CXCursor cursor;
  cursor = Cxcursor_val(Field(cursor_ocaml, 0));
  CXCursor result = clang_getCursorLexicalParent(cursor);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxcursor(result));
  Store_field(data, 1, Field(cursor_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getOverriddenCursors_wrapper(value cursor_ocaml)
{
  CAMLparam1(cursor_ocaml);
  CXCursor cursor;
  cursor = Cxcursor_val(Field(cursor_ocaml, 0));
  unsigned int num_overridden;
  CXCursor * overridden;
  clang_getOverriddenCursors(cursor, &overridden, &num_overridden);
  {
    CAMLlocal1(data);
    data = caml_alloc(num_overridden, 0);
for (unsigned int i = 0; i < num_overridden; i++) {
  CAMLlocal1(cell);
  cell = caml_alloc_tuple(2);
  Store_field(cell, 0, Val_cxcursor(overridden[i]));
  Store_field(cell, 1, Field(cursor_ocaml, 1));
  Store_field(data, i, cell);
}
clang_disposeOverriddenCursors(overridden);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getIncludedFile_wrapper(value cursor_ocaml)
{
  CAMLparam1(cursor_ocaml);
  CXCursor cursor;
  cursor = Cxcursor_val(Field(cursor_ocaml, 0));
  CXFile result = clang_getIncludedFile(cursor);
  {
    CAMLlocal1(data);
    data = Val_cxfile(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCursor_wrapper(value arg_ocaml, value arg2_ocaml)
{
  CAMLparam2(arg_ocaml, arg2_ocaml);
  CXTranslationUnit arg;
  arg = Cxtranslationunit_val(Field(arg_ocaml, 0));
  CXSourceLocation arg2;
  arg2 = Cxsourcelocation_val(arg2_ocaml);
  CXCursor result = clang_getCursor(arg, arg2);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxcursor(result));
  Store_field(data, 1, arg_ocaml);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCursorLocation_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXCursor arg;
  arg = Cxcursor_val(Field(arg_ocaml, 0));
  CXSourceLocation result = clang_getCursorLocation(arg);
  {
    CAMLlocal1(data);
    data = Val_cxsourcelocation(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCursorExtent_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXCursor arg;
  arg = Cxcursor_val(Field(arg_ocaml, 0));
  CXSourceRange result = clang_getCursorExtent(arg);
  {
    CAMLlocal1(data);
    data = Val_cxsourcerange(result);
    CAMLreturn(data);
  }
}

enum CXTypeKind
Cxtypekind_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return CXType_Invalid;
  case 1: return CXType_Unexposed;
  case 2: return CXType_Void;
  case 3: return CXType_Bool;
  case 4: return CXType_Char_U;
  case 5: return CXType_UChar;
  case 6: return CXType_Char16;
  case 7: return CXType_Char32;
  case 8: return CXType_UShort;
  case 9: return CXType_UInt;
  case 10: return CXType_ULong;
  case 11: return CXType_ULongLong;
  case 12: return CXType_UInt128;
  case 13: return CXType_Char_S;
  case 14: return CXType_SChar;
  case 15: return CXType_WChar;
  case 16: return CXType_Short;
  case 17: return CXType_Int;
  case 18: return CXType_Long;
  case 19: return CXType_LongLong;
  case 20: return CXType_Int128;
  case 21: return CXType_Float;
  case 22: return CXType_Double;
  case 23: return CXType_LongDouble;
  case 24: return CXType_NullPtr;
  case 25: return CXType_Overload;
  case 26: return CXType_Dependent;
  case 27: return CXType_ObjCId;
  case 28: return CXType_ObjCClass;
  case 29: return CXType_ObjCSel;
  case 30: return CXType_Float128;
  case 31: return CXType_Half;
  case 32: return CXType_Float16;
  case 33: return CXType_ShortAccum;
  case 34: return CXType_Accum;
  case 35: return CXType_LongAccum;
  case 36: return CXType_UShortAccum;
  case 37: return CXType_UAccum;
  case 38: return CXType_ULongAccum;
  case 39: return CXType_Complex;
  case 40: return CXType_Pointer;
  case 41: return CXType_BlockPointer;
  case 42: return CXType_LValueReference;
  case 43: return CXType_RValueReference;
  case 44: return CXType_Record;
  case 45: return CXType_Enum;
  case 46: return CXType_Typedef;
  case 47: return CXType_ObjCInterface;
  case 48: return CXType_ObjCObjectPointer;
  case 49: return CXType_FunctionNoProto;
  case 50: return CXType_FunctionProto;
  case 51: return CXType_ConstantArray;
  case 52: return CXType_Vector;
  case 53: return CXType_IncompleteArray;
  case 54: return CXType_VariableArray;
  case 55: return CXType_DependentSizedArray;
  case 56: return CXType_MemberPointer;
  case 57: return CXType_Auto;
  case 58: return CXType_Elaborated;
  case 59: return CXType_Pipe;
  case 60: return CXType_OCLImage1dRO;
  case 61: return CXType_OCLImage1dArrayRO;
  case 62: return CXType_OCLImage1dBufferRO;
  case 63: return CXType_OCLImage2dRO;
  case 64: return CXType_OCLImage2dArrayRO;
  case 65: return CXType_OCLImage2dDepthRO;
  case 66: return CXType_OCLImage2dArrayDepthRO;
  case 67: return CXType_OCLImage2dMSAARO;
  case 68: return CXType_OCLImage2dArrayMSAARO;
  case 69: return CXType_OCLImage2dMSAADepthRO;
  case 70: return CXType_OCLImage2dArrayMSAADepthRO;
  case 71: return CXType_OCLImage3dRO;
  case 72: return CXType_OCLImage1dWO;
  case 73: return CXType_OCLImage1dArrayWO;
  case 74: return CXType_OCLImage1dBufferWO;
  case 75: return CXType_OCLImage2dWO;
  case 76: return CXType_OCLImage2dArrayWO;
  case 77: return CXType_OCLImage2dDepthWO;
  case 78: return CXType_OCLImage2dArrayDepthWO;
  case 79: return CXType_OCLImage2dMSAAWO;
  case 80: return CXType_OCLImage2dArrayMSAAWO;
  case 81: return CXType_OCLImage2dMSAADepthWO;
  case 82: return CXType_OCLImage2dArrayMSAADepthWO;
  case 83: return CXType_OCLImage3dWO;
  case 84: return CXType_OCLImage1dRW;
  case 85: return CXType_OCLImage1dArrayRW;
  case 86: return CXType_OCLImage1dBufferRW;
  case 87: return CXType_OCLImage2dRW;
  case 88: return CXType_OCLImage2dArrayRW;
  case 89: return CXType_OCLImage2dDepthRW;
  case 90: return CXType_OCLImage2dArrayDepthRW;
  case 91: return CXType_OCLImage2dMSAARW;
  case 92: return CXType_OCLImage2dArrayMSAARW;
  case 93: return CXType_OCLImage2dMSAADepthRW;
  case 94: return CXType_OCLImage2dArrayMSAADepthRW;
  case 95: return CXType_OCLImage3dRW;
  case 96: return CXType_OCLSampler;
  case 97: return CXType_OCLEvent;
  case 98: return CXType_OCLQueue;
  case 99: return CXType_OCLReserveID;
  }
  failwith_fmt("invalid value for Cxtypekind_val: %d", Int_val(ocaml));
  return CXType_Invalid;
}

value
Val_cxtypekind(enum CXTypeKind v)
{
  switch (v) {
  case CXType_Invalid: return Val_int(0);
  case CXType_Unexposed: return Val_int(1);
  case CXType_Void: return Val_int(2);
  case CXType_Bool: return Val_int(3);
  case CXType_Char_U: return Val_int(4);
  case CXType_UChar: return Val_int(5);
  case CXType_Char16: return Val_int(6);
  case CXType_Char32: return Val_int(7);
  case CXType_UShort: return Val_int(8);
  case CXType_UInt: return Val_int(9);
  case CXType_ULong: return Val_int(10);
  case CXType_ULongLong: return Val_int(11);
  case CXType_UInt128: return Val_int(12);
  case CXType_Char_S: return Val_int(13);
  case CXType_SChar: return Val_int(14);
  case CXType_WChar: return Val_int(15);
  case CXType_Short: return Val_int(16);
  case CXType_Int: return Val_int(17);
  case CXType_Long: return Val_int(18);
  case CXType_LongLong: return Val_int(19);
  case CXType_Int128: return Val_int(20);
  case CXType_Float: return Val_int(21);
  case CXType_Double: return Val_int(22);
  case CXType_LongDouble: return Val_int(23);
  case CXType_NullPtr: return Val_int(24);
  case CXType_Overload: return Val_int(25);
  case CXType_Dependent: return Val_int(26);
  case CXType_ObjCId: return Val_int(27);
  case CXType_ObjCClass: return Val_int(28);
  case CXType_ObjCSel: return Val_int(29);
  case CXType_Float128: return Val_int(30);
  case CXType_Half: return Val_int(31);
  case CXType_Float16: return Val_int(32);
  case CXType_ShortAccum: return Val_int(33);
  case CXType_Accum: return Val_int(34);
  case CXType_LongAccum: return Val_int(35);
  case CXType_UShortAccum: return Val_int(36);
  case CXType_UAccum: return Val_int(37);
  case CXType_ULongAccum: return Val_int(38);
  case CXType_Complex: return Val_int(39);
  case CXType_Pointer: return Val_int(40);
  case CXType_BlockPointer: return Val_int(41);
  case CXType_LValueReference: return Val_int(42);
  case CXType_RValueReference: return Val_int(43);
  case CXType_Record: return Val_int(44);
  case CXType_Enum: return Val_int(45);
  case CXType_Typedef: return Val_int(46);
  case CXType_ObjCInterface: return Val_int(47);
  case CXType_ObjCObjectPointer: return Val_int(48);
  case CXType_FunctionNoProto: return Val_int(49);
  case CXType_FunctionProto: return Val_int(50);
  case CXType_ConstantArray: return Val_int(51);
  case CXType_Vector: return Val_int(52);
  case CXType_IncompleteArray: return Val_int(53);
  case CXType_VariableArray: return Val_int(54);
  case CXType_DependentSizedArray: return Val_int(55);
  case CXType_MemberPointer: return Val_int(56);
  case CXType_Auto: return Val_int(57);
  case CXType_Elaborated: return Val_int(58);
  case CXType_Pipe: return Val_int(59);
  case CXType_OCLImage1dRO: return Val_int(60);
  case CXType_OCLImage1dArrayRO: return Val_int(61);
  case CXType_OCLImage1dBufferRO: return Val_int(62);
  case CXType_OCLImage2dRO: return Val_int(63);
  case CXType_OCLImage2dArrayRO: return Val_int(64);
  case CXType_OCLImage2dDepthRO: return Val_int(65);
  case CXType_OCLImage2dArrayDepthRO: return Val_int(66);
  case CXType_OCLImage2dMSAARO: return Val_int(67);
  case CXType_OCLImage2dArrayMSAARO: return Val_int(68);
  case CXType_OCLImage2dMSAADepthRO: return Val_int(69);
  case CXType_OCLImage2dArrayMSAADepthRO: return Val_int(70);
  case CXType_OCLImage3dRO: return Val_int(71);
  case CXType_OCLImage1dWO: return Val_int(72);
  case CXType_OCLImage1dArrayWO: return Val_int(73);
  case CXType_OCLImage1dBufferWO: return Val_int(74);
  case CXType_OCLImage2dWO: return Val_int(75);
  case CXType_OCLImage2dArrayWO: return Val_int(76);
  case CXType_OCLImage2dDepthWO: return Val_int(77);
  case CXType_OCLImage2dArrayDepthWO: return Val_int(78);
  case CXType_OCLImage2dMSAAWO: return Val_int(79);
  case CXType_OCLImage2dArrayMSAAWO: return Val_int(80);
  case CXType_OCLImage2dMSAADepthWO: return Val_int(81);
  case CXType_OCLImage2dArrayMSAADepthWO: return Val_int(82);
  case CXType_OCLImage3dWO: return Val_int(83);
  case CXType_OCLImage1dRW: return Val_int(84);
  case CXType_OCLImage1dArrayRW: return Val_int(85);
  case CXType_OCLImage1dBufferRW: return Val_int(86);
  case CXType_OCLImage2dRW: return Val_int(87);
  case CXType_OCLImage2dArrayRW: return Val_int(88);
  case CXType_OCLImage2dDepthRW: return Val_int(89);
  case CXType_OCLImage2dArrayDepthRW: return Val_int(90);
  case CXType_OCLImage2dMSAARW: return Val_int(91);
  case CXType_OCLImage2dArrayMSAARW: return Val_int(92);
  case CXType_OCLImage2dMSAADepthRW: return Val_int(93);
  case CXType_OCLImage2dArrayMSAADepthRW: return Val_int(94);
  case CXType_OCLImage3dRW: return Val_int(95);
  case CXType_OCLSampler: return Val_int(96);
  case CXType_OCLEvent: return Val_int(97);
  case CXType_OCLQueue: return Val_int(98);
  case CXType_OCLReserveID: return Val_int(99);
  }
  failwith_fmt("invalid value for Val_cxtypekind: %d", v);
  return Val_int(0);
}

DECLARE_OPAQUE(CXType, cxtype, Cxtype_val, Val_cxtype, custom_finalize_default)

CAMLprim value
clang_getTypeKind_wrapper(value arg_ocaml)
{
CAMLparam1(arg_ocaml);
CXType arg;
arg = Cxtype_val(Field(arg_ocaml, 0));
  
  enum CXTypeKind result = arg.kind;
  {
    CAMLlocal1(data);
    data = Val_cxtypekind(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCursorType_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  CXType result = clang_getCursorType(C);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxtype(result));
  Store_field(data, 1, Field(C_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getTypeSpelling_wrapper(value CT_ocaml)
{
  CAMLparam1(CT_ocaml);
  CXType CT;
  CT = Cxtype_val(Field(CT_ocaml, 0));
  CXString result = clang_getTypeSpelling(CT);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getTypedefDeclUnderlyingType_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  CXType result = clang_getTypedefDeclUnderlyingType(C);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxtype(result));
  Store_field(data, 1, Field(C_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getEnumDeclIntegerType_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  CXType result = clang_getEnumDeclIntegerType(C);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxtype(result));
  Store_field(data, 1, Field(C_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getEnumConstantDeclValue_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  long long result = clang_getEnumConstantDeclValue(C);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getEnumConstantDeclUnsignedValue_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned long long result = clang_getEnumConstantDeclUnsignedValue(C);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getFieldDeclBitWidth_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  int result = clang_getFieldDeclBitWidth(C);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_getNumArguments_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  int result = clang_Cursor_getNumArguments(C);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_getArgument_wrapper(value C_ocaml, value i_ocaml)
{
  CAMLparam2(C_ocaml, i_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int i;
  i = Int_val(i_ocaml);
  CXCursor result = clang_Cursor_getArgument(C, i);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxcursor(result));
  Store_field(data, 1, Field(C_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_getNumTemplateArguments_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  int result = clang_Cursor_getNumTemplateArguments(C);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

enum CXTemplateArgumentKind
Cxtemplateargumentkind_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return CXTemplateArgumentKind_Null;
  case 1: return CXTemplateArgumentKind_Type;
  case 2: return CXTemplateArgumentKind_Declaration;
  case 3: return CXTemplateArgumentKind_NullPtr;
  case 4: return CXTemplateArgumentKind_Integral;
  case 5: return CXTemplateArgumentKind_Template;
  case 6: return CXTemplateArgumentKind_TemplateExpansion;
  case 7: return CXTemplateArgumentKind_Expression;
  case 8: return CXTemplateArgumentKind_Pack;
  case 9: return CXTemplateArgumentKind_Invalid;
  }
  failwith_fmt("invalid value for Cxtemplateargumentkind_val: %d", Int_val(ocaml));
  return CXTemplateArgumentKind_Null;
}

value
Val_cxtemplateargumentkind(enum CXTemplateArgumentKind v)
{
  switch (v) {
  case CXTemplateArgumentKind_Null: return Val_int(0);
  case CXTemplateArgumentKind_Type: return Val_int(1);
  case CXTemplateArgumentKind_Declaration: return Val_int(2);
  case CXTemplateArgumentKind_NullPtr: return Val_int(3);
  case CXTemplateArgumentKind_Integral: return Val_int(4);
  case CXTemplateArgumentKind_Template: return Val_int(5);
  case CXTemplateArgumentKind_TemplateExpansion: return Val_int(6);
  case CXTemplateArgumentKind_Expression: return Val_int(7);
  case CXTemplateArgumentKind_Pack: return Val_int(8);
  case CXTemplateArgumentKind_Invalid: return Val_int(9);
  }
  failwith_fmt("invalid value for Val_cxtemplateargumentkind: %d", v);
  return Val_int(0);
}

CAMLprim value
clang_Cursor_getTemplateArgumentKind_wrapper(value C_ocaml, value I_ocaml)
{
  CAMLparam2(C_ocaml, I_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int I;
  I = Int_val(I_ocaml);
  enum CXTemplateArgumentKind result = clang_Cursor_getTemplateArgumentKind(C, I);
  {
    CAMLlocal1(data);
    data = Val_cxtemplateargumentkind(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_getTemplateArgumentType_wrapper(value C_ocaml, value I_ocaml)
{
  CAMLparam2(C_ocaml, I_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int I;
  I = Int_val(I_ocaml);
  CXType result = clang_Cursor_getTemplateArgumentType(C, I);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxtype(result));
  Store_field(data, 1, Field(C_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_getTemplateArgumentValue_wrapper(value C_ocaml, value I_ocaml)
{
  CAMLparam2(C_ocaml, I_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int I;
  I = Int_val(I_ocaml);
  long long result = clang_Cursor_getTemplateArgumentValue(C, I);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_getTemplateArgumentUnsignedValue_wrapper(value C_ocaml, value I_ocaml)
{
  CAMLparam2(C_ocaml, I_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int I;
  I = Int_val(I_ocaml);
  unsigned long long result = clang_Cursor_getTemplateArgumentUnsignedValue(C, I);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_equalTypes_wrapper(value A_ocaml, value B_ocaml)
{
  CAMLparam2(A_ocaml, B_ocaml);
  CXType A;
  A = Cxtype_val(Field(A_ocaml, 0));
  CXType B;
  B = Cxtype_val(Field(B_ocaml, 0));
  unsigned int result = clang_equalTypes(A, B);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCanonicalType_wrapper(value T_ocaml)
{
  CAMLparam1(T_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  CXType result = clang_getCanonicalType(T);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxtype(result));
  Store_field(data, 1, Field(T_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_isConstQualifiedType_wrapper(value T_ocaml)
{
  CAMLparam1(T_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  unsigned int result = clang_isConstQualifiedType(T);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_isMacroFunctionLike_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int result = clang_Cursor_isMacroFunctionLike(C);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_isMacroBuiltin_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int result = clang_Cursor_isMacroBuiltin(C);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_isFunctionInlined_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int result = clang_Cursor_isFunctionInlined(C);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_isVolatileQualifiedType_wrapper(value T_ocaml)
{
  CAMLparam1(T_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  unsigned int result = clang_isVolatileQualifiedType(T);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_isRestrictQualifiedType_wrapper(value T_ocaml)
{
  CAMLparam1(T_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  unsigned int result = clang_isRestrictQualifiedType(T);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getAddressSpace_wrapper(value T_ocaml)
{
  CAMLparam1(T_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  unsigned int result = clang_getAddressSpace(T);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getTypedefName_wrapper(value CT_ocaml)
{
  CAMLparam1(CT_ocaml);
  CXType CT;
  CT = Cxtype_val(Field(CT_ocaml, 0));
  CXString result = clang_getTypedefName(CT);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getPointeeType_wrapper(value T_ocaml)
{
  CAMLparam1(T_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  CXType result = clang_getPointeeType(T);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxtype(result));
  Store_field(data, 1, Field(T_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getTypeDeclaration_wrapper(value T_ocaml)
{
  CAMLparam1(T_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  CXCursor result = clang_getTypeDeclaration(T);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxcursor(result));
  Store_field(data, 1, Field(T_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getDeclObjCTypeEncoding_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  CXString result = clang_getDeclObjCTypeEncoding(C);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Type_getObjCEncoding_wrapper(value type_ocaml)
{
  CAMLparam1(type_ocaml);
  CXType type;
  type = Cxtype_val(Field(type_ocaml, 0));
  CXString result = clang_Type_getObjCEncoding(type);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getTypeKindSpelling_wrapper(value K_ocaml)
{
  CAMLparam1(K_ocaml);
  enum CXTypeKind K;
  K = Cxtypekind_val(K_ocaml);
  CXString result = clang_getTypeKindSpelling(K);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

enum CXCallingConv
Cxcallingconv_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return CXCallingConv_Default;
  case 1: return CXCallingConv_C;
  case 2: return CXCallingConv_X86StdCall;
  case 3: return CXCallingConv_X86FastCall;
  case 4: return CXCallingConv_X86ThisCall;
  case 5: return CXCallingConv_X86Pascal;
  case 6: return CXCallingConv_AAPCS;
  case 7: return CXCallingConv_AAPCS_VFP;
  case 8: return CXCallingConv_X86RegCall;
  case 9: return CXCallingConv_IntelOclBicc;
  case 10: return CXCallingConv_Win64;
  case 11: return CXCallingConv_X86_64SysV;
  case 12: return CXCallingConv_X86VectorCall;
  case 13: return CXCallingConv_Swift;
  case 14: return CXCallingConv_PreserveMost;
  case 15: return CXCallingConv_PreserveAll;
  case 16: return CXCallingConv_Invalid;
  case 17: return CXCallingConv_Unexposed;
  }
  failwith_fmt("invalid value for Cxcallingconv_val: %d", Int_val(ocaml));
  return CXCallingConv_Default;
}

value
Val_cxcallingconv(enum CXCallingConv v)
{
  switch (v) {
  case CXCallingConv_Default: return Val_int(0);
  case CXCallingConv_C: return Val_int(1);
  case CXCallingConv_X86StdCall: return Val_int(2);
  case CXCallingConv_X86FastCall: return Val_int(3);
  case CXCallingConv_X86ThisCall: return Val_int(4);
  case CXCallingConv_X86Pascal: return Val_int(5);
  case CXCallingConv_AAPCS: return Val_int(6);
  case CXCallingConv_AAPCS_VFP: return Val_int(7);
  case CXCallingConv_X86RegCall: return Val_int(8);
  case CXCallingConv_IntelOclBicc: return Val_int(9);
  case CXCallingConv_Win64: return Val_int(10);
  case CXCallingConv_X86_64SysV: return Val_int(11);
  case CXCallingConv_X86VectorCall: return Val_int(12);
  case CXCallingConv_Swift: return Val_int(13);
  case CXCallingConv_PreserveMost: return Val_int(14);
  case CXCallingConv_PreserveAll: return Val_int(15);
  case CXCallingConv_Invalid: return Val_int(16);
  case CXCallingConv_Unexposed: return Val_int(17);
  }
  failwith_fmt("invalid value for Val_cxcallingconv: %d", v);
  return Val_int(0);
}

CAMLprim value
clang_getFunctionTypeCallingConv_wrapper(value T_ocaml)
{
  CAMLparam1(T_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  enum CXCallingConv result = clang_getFunctionTypeCallingConv(T);
  {
    CAMLlocal1(data);
    data = Val_cxcallingconv(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getResultType_wrapper(value T_ocaml)
{
  CAMLparam1(T_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  CXType result = clang_getResultType(T);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxtype(result));
  Store_field(data, 1, Field(T_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getExceptionSpecificationType_wrapper(value T_ocaml)
{
  CAMLparam1(T_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  int result = clang_getExceptionSpecificationType(T);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getNumArgTypes_wrapper(value T_ocaml)
{
  CAMLparam1(T_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  int result = clang_getNumArgTypes(T);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getArgType_wrapper(value T_ocaml, value i_ocaml)
{
  CAMLparam2(T_ocaml, i_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  unsigned int i;
  i = Int_val(i_ocaml);
  CXType result = clang_getArgType(T, i);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxtype(result));
  Store_field(data, 1, Field(T_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_isFunctionTypeVariadic_wrapper(value T_ocaml)
{
  CAMLparam1(T_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  unsigned int result = clang_isFunctionTypeVariadic(T);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCursorResultType_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  CXType result = clang_getCursorResultType(C);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxtype(result));
  Store_field(data, 1, Field(C_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCursorExceptionSpecificationType_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  int result = clang_getCursorExceptionSpecificationType(C);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_isPODType_wrapper(value T_ocaml)
{
  CAMLparam1(T_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  unsigned int result = clang_isPODType(T);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getElementType_wrapper(value T_ocaml)
{
  CAMLparam1(T_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  CXType result = clang_getElementType(T);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxtype(result));
  Store_field(data, 1, Field(T_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getNumElements_wrapper(value T_ocaml)
{
  CAMLparam1(T_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  long long result = clang_getNumElements(T);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getArrayElementType_wrapper(value T_ocaml)
{
  CAMLparam1(T_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  CXType result = clang_getArrayElementType(T);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxtype(result));
  Store_field(data, 1, Field(T_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getArraySize_wrapper(value T_ocaml)
{
  CAMLparam1(T_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  long long result = clang_getArraySize(T);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Type_getNamedType_wrapper(value T_ocaml)
{
  CAMLparam1(T_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  CXType result = clang_Type_getNamedType(T);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxtype(result));
  Store_field(data, 1, Field(T_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Type_isTransparentTagTypedef_wrapper(value T_ocaml)
{
  CAMLparam1(T_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  unsigned int result = clang_Type_isTransparentTagTypedef(T);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Type_getAlignOf_wrapper(value T_ocaml)
{
  CAMLparam1(T_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  long long result = clang_Type_getAlignOf(T);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Type_getClassType_wrapper(value T_ocaml)
{
  CAMLparam1(T_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  CXType result = clang_Type_getClassType(T);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxtype(result));
  Store_field(data, 1, Field(T_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Type_getSizeOf_wrapper(value T_ocaml)
{
  CAMLparam1(T_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  long long result = clang_Type_getSizeOf(T);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Type_getOffsetOf_wrapper(value T_ocaml, value S_ocaml)
{
  CAMLparam2(T_ocaml, S_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  const char * S;
  S = String_val(S_ocaml);
  long long result = clang_Type_getOffsetOf(T, S);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_getOffsetOfField_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  long long result = clang_Cursor_getOffsetOfField(C);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_isAnonymous_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int result = clang_Cursor_isAnonymous(C);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Type_getNumTemplateArguments_wrapper(value T_ocaml)
{
  CAMLparam1(T_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  int result = clang_Type_getNumTemplateArguments(T);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Type_getTemplateArgumentAsType_wrapper(value T_ocaml, value i_ocaml)
{
  CAMLparam2(T_ocaml, i_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  unsigned int i;
  i = Int_val(i_ocaml);
  CXType result = clang_Type_getTemplateArgumentAsType(T, i);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxtype(result));
  Store_field(data, 1, Field(T_ocaml, 1));
    CAMLreturn(data);
  }
}

enum CXRefQualifierKind
Cxrefqualifierkind_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return CXRefQualifier_None;
  case 1: return CXRefQualifier_LValue;
  case 2: return CXRefQualifier_RValue;
  }
  failwith_fmt("invalid value for Cxrefqualifierkind_val: %d", Int_val(ocaml));
  return CXRefQualifier_None;
}

value
Val_cxrefqualifierkind(enum CXRefQualifierKind v)
{
  switch (v) {
  case CXRefQualifier_None: return Val_int(0);
  case CXRefQualifier_LValue: return Val_int(1);
  case CXRefQualifier_RValue: return Val_int(2);
  }
  failwith_fmt("invalid value for Val_cxrefqualifierkind: %d", v);
  return Val_int(0);
}

CAMLprim value
clang_Type_getCXXRefQualifier_wrapper(value T_ocaml)
{
  CAMLparam1(T_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  enum CXRefQualifierKind result = clang_Type_getCXXRefQualifier(T);
  {
    CAMLlocal1(data);
    data = Val_cxrefqualifierkind(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_isBitField_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int result = clang_Cursor_isBitField(C);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_isVirtualBase_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXCursor arg;
  arg = Cxcursor_val(Field(arg_ocaml, 0));
  unsigned int result = clang_isVirtualBase(arg);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

enum CX_CXXAccessSpecifier
Cx_cxxaccessspecifier_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return CX_CXXInvalidAccessSpecifier;
  case 1: return CX_CXXPublic;
  case 2: return CX_CXXProtected;
  case 3: return CX_CXXPrivate;
  }
  failwith_fmt("invalid value for Cx_cxxaccessspecifier_val: %d", Int_val(ocaml));
  return CX_CXXInvalidAccessSpecifier;
}

value
Val_cx_cxxaccessspecifier(enum CX_CXXAccessSpecifier v)
{
  switch (v) {
  case CX_CXXInvalidAccessSpecifier: return Val_int(0);
  case CX_CXXPublic: return Val_int(1);
  case CX_CXXProtected: return Val_int(2);
  case CX_CXXPrivate: return Val_int(3);
  }
  failwith_fmt("invalid value for Val_cx_cxxaccessspecifier: %d", v);
  return Val_int(0);
}

CAMLprim value
clang_getCXXAccessSpecifier_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXCursor arg;
  arg = Cxcursor_val(Field(arg_ocaml, 0));
  enum CX_CXXAccessSpecifier result = clang_getCXXAccessSpecifier(arg);
  {
    CAMLlocal1(data);
    data = Val_cx_cxxaccessspecifier(result);
    CAMLreturn(data);
  }
}

enum CX_StorageClass
Cx_storageclass_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return CX_SC_Invalid;
  case 1: return CX_SC_None;
  case 2: return CX_SC_Extern;
  case 3: return CX_SC_Static;
  case 4: return CX_SC_PrivateExtern;
  case 5: return CX_SC_OpenCLWorkGroupLocal;
  case 6: return CX_SC_Auto;
  case 7: return CX_SC_Register;
  }
  failwith_fmt("invalid value for Cx_storageclass_val: %d", Int_val(ocaml));
  return CX_SC_Invalid;
}

value
Val_cx_storageclass(enum CX_StorageClass v)
{
  switch (v) {
  case CX_SC_Invalid: return Val_int(0);
  case CX_SC_None: return Val_int(1);
  case CX_SC_Extern: return Val_int(2);
  case CX_SC_Static: return Val_int(3);
  case CX_SC_PrivateExtern: return Val_int(4);
  case CX_SC_OpenCLWorkGroupLocal: return Val_int(5);
  case CX_SC_Auto: return Val_int(6);
  case CX_SC_Register: return Val_int(7);
  }
  failwith_fmt("invalid value for Val_cx_storageclass: %d", v);
  return Val_int(0);
}

CAMLprim value
clang_Cursor_getStorageClass_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXCursor arg;
  arg = Cxcursor_val(Field(arg_ocaml, 0));
  enum CX_StorageClass result = clang_Cursor_getStorageClass(arg);
  {
    CAMLlocal1(data);
    data = Val_cx_storageclass(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getNumOverloadedDecls_wrapper(value cursor_ocaml)
{
  CAMLparam1(cursor_ocaml);
  CXCursor cursor;
  cursor = Cxcursor_val(Field(cursor_ocaml, 0));
  unsigned int result = clang_getNumOverloadedDecls(cursor);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getOverloadedDecl_wrapper(value cursor_ocaml, value index_ocaml)
{
  CAMLparam2(cursor_ocaml, index_ocaml);
  CXCursor cursor;
  cursor = Cxcursor_val(Field(cursor_ocaml, 0));
  unsigned int index;
  index = Int_val(index_ocaml);
  CXCursor result = clang_getOverloadedDecl(cursor, index);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxcursor(result));
  Store_field(data, 1, Field(cursor_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getIBOutletCollectionType_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXCursor arg;
  arg = Cxcursor_val(Field(arg_ocaml, 0));
  CXType result = clang_getIBOutletCollectionType(arg);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxtype(result));
  Store_field(data, 1, Field(arg_ocaml, 1));
    CAMLreturn(data);
  }
}

enum CXChildVisitResult
Cxchildvisitresult_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return CXChildVisit_Break;
  case 1: return CXChildVisit_Continue;
  case 2: return CXChildVisit_Recurse;
  }
  failwith_fmt("invalid value for Cxchildvisitresult_val: %d", Int_val(ocaml));
  return CXChildVisit_Break;
}

value
Val_cxchildvisitresult(enum CXChildVisitResult v)
{
  switch (v) {
  case CXChildVisit_Break: return Val_int(0);
  case CXChildVisit_Continue: return Val_int(1);
  case CXChildVisit_Recurse: return Val_int(2);
  }
  failwith_fmt("invalid value for Val_cxchildvisitresult: %d", v);
  return Val_int(0);
}

enum CXChildVisitResult
clang_visitChildren_visitor_callback(CXCursor arg0, CXCursor arg1, CXClientData arg2)
{
  CAMLparam0();
  CAMLlocal4(result, f, arg0_ocaml, arg1_ocaml);
  f = *((value *) ((value **)arg2)[0]);
arg0_ocaml = caml_alloc_tuple(2);
  Store_field(arg0_ocaml, 0, Val_cxcursor(arg0));
  Store_field(arg0_ocaml, 1, *((value **)arg2)[1]);arg1_ocaml = caml_alloc_tuple(2);
  Store_field(arg1_ocaml, 0, Val_cxcursor(arg1));
  Store_field(arg1_ocaml, 1, *((value **)arg2)[1]);  result = caml_callback2(f, arg0_ocaml, arg1_ocaml);
  {
    CAMLlocal1(data);
    data = Cxchildvisitresult_val(result);
    CAMLreturnT(enum CXChildVisitResult, data);
  }

}

CAMLprim value
clang_visitChildren_wrapper(value parent_ocaml, value visitor_ocaml)
{
  CAMLparam2(parent_ocaml, visitor_ocaml);
  CXCursor parent;
  parent = Cxcursor_val(Field(parent_ocaml, 0));
  unsigned int result = clang_visitChildren(parent, clang_visitChildren_visitor_callback, (value *[]){&visitor_ocaml,&parent_ocaml});
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCursorUSR_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXCursor arg;
  arg = Cxcursor_val(Field(arg_ocaml, 0));
  CXString result = clang_getCursorUSR(arg);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCursorSpelling_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXCursor arg;
  arg = Cxcursor_val(Field(arg_ocaml, 0));
  CXString result = clang_getCursorSpelling(arg);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_getSpellingNameRange_wrapper(value arg_ocaml, value pieceIndex_ocaml, value options_ocaml)
{
  CAMLparam3(arg_ocaml, pieceIndex_ocaml, options_ocaml);
  CXCursor arg;
  arg = Cxcursor_val(Field(arg_ocaml, 0));
  unsigned int pieceIndex;
  pieceIndex = Int_val(pieceIndex_ocaml);
  unsigned int options;
  options = Int_val(options_ocaml);
  CXSourceRange result = clang_Cursor_getSpellingNameRange(arg, pieceIndex, options);
  {
    CAMLlocal1(data);
    data = Val_cxsourcerange(result);
    CAMLreturn(data);
  }
}

DECLARE_OPAQUE(CXPrintingPolicy, cxprintingpolicy, Cxprintingpolicy_val, Val_cxprintingpolicy, custom_finalize_default)

enum CXPrintingPolicyProperty
Cxprintingpolicyproperty_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return CXPrintingPolicy_Indentation;
  case 1: return CXPrintingPolicy_SuppressSpecifiers;
  case 2: return CXPrintingPolicy_SuppressTagKeyword;
  case 3: return CXPrintingPolicy_IncludeTagDefinition;
  case 4: return CXPrintingPolicy_SuppressScope;
  case 5: return CXPrintingPolicy_SuppressUnwrittenScope;
  case 6: return CXPrintingPolicy_SuppressInitializers;
  case 7: return CXPrintingPolicy_ConstantArraySizeAsWritten;
  case 8: return CXPrintingPolicy_AnonymousTagLocations;
  case 9: return CXPrintingPolicy_SuppressStrongLifetime;
  case 10: return CXPrintingPolicy_SuppressLifetimeQualifiers;
  case 11: return CXPrintingPolicy_SuppressTemplateArgsInCXXConstructors;
  case 12: return CXPrintingPolicy_Bool;
  case 13: return CXPrintingPolicy_Restrict;
  case 14: return CXPrintingPolicy_Alignof;
  case 15: return CXPrintingPolicy_UnderscoreAlignof;
  case 16: return CXPrintingPolicy_UseVoidForZeroParams;
  case 17: return CXPrintingPolicy_TerseOutput;
  case 18: return CXPrintingPolicy_PolishForDeclaration;
  case 19: return CXPrintingPolicy_Half;
  case 20: return CXPrintingPolicy_MSWChar;
  case 21: return CXPrintingPolicy_IncludeNewlines;
  case 22: return CXPrintingPolicy_MSVCFormatting;
  case 23: return CXPrintingPolicy_ConstantsAsWritten;
  case 24: return CXPrintingPolicy_SuppressImplicitBase;
  case 25: return CXPrintingPolicy_FullyQualifiedName;
  }
  failwith_fmt("invalid value for Cxprintingpolicyproperty_val: %d", Int_val(ocaml));
  return CXPrintingPolicy_Indentation;
}

value
Val_cxprintingpolicyproperty(enum CXPrintingPolicyProperty v)
{
  switch (v) {
  case CXPrintingPolicy_Indentation: return Val_int(0);
  case CXPrintingPolicy_SuppressSpecifiers: return Val_int(1);
  case CXPrintingPolicy_SuppressTagKeyword: return Val_int(2);
  case CXPrintingPolicy_IncludeTagDefinition: return Val_int(3);
  case CXPrintingPolicy_SuppressScope: return Val_int(4);
  case CXPrintingPolicy_SuppressUnwrittenScope: return Val_int(5);
  case CXPrintingPolicy_SuppressInitializers: return Val_int(6);
  case CXPrintingPolicy_ConstantArraySizeAsWritten: return Val_int(7);
  case CXPrintingPolicy_AnonymousTagLocations: return Val_int(8);
  case CXPrintingPolicy_SuppressStrongLifetime: return Val_int(9);
  case CXPrintingPolicy_SuppressLifetimeQualifiers: return Val_int(10);
  case CXPrintingPolicy_SuppressTemplateArgsInCXXConstructors: return Val_int(11);
  case CXPrintingPolicy_Bool: return Val_int(12);
  case CXPrintingPolicy_Restrict: return Val_int(13);
  case CXPrintingPolicy_Alignof: return Val_int(14);
  case CXPrintingPolicy_UnderscoreAlignof: return Val_int(15);
  case CXPrintingPolicy_UseVoidForZeroParams: return Val_int(16);
  case CXPrintingPolicy_TerseOutput: return Val_int(17);
  case CXPrintingPolicy_PolishForDeclaration: return Val_int(18);
  case CXPrintingPolicy_Half: return Val_int(19);
  case CXPrintingPolicy_MSWChar: return Val_int(20);
  case CXPrintingPolicy_IncludeNewlines: return Val_int(21);
  case CXPrintingPolicy_MSVCFormatting: return Val_int(22);
  case CXPrintingPolicy_ConstantsAsWritten: return Val_int(23);
  case CXPrintingPolicy_SuppressImplicitBase: return Val_int(24);
  case CXPrintingPolicy_FullyQualifiedName: return Val_int(25);
  }
  failwith_fmt("invalid value for Val_cxprintingpolicyproperty: %d", v);
  return Val_int(0);
}

CAMLprim value
clang_PrintingPolicy_getProperty_wrapper(value Policy_ocaml, value Property_ocaml)
{
  CAMLparam2(Policy_ocaml, Property_ocaml);
  CXPrintingPolicy Policy;
  Policy = Cxprintingpolicy_val(Policy_ocaml);
  enum CXPrintingPolicyProperty Property;
  Property = Cxprintingpolicyproperty_val(Property_ocaml);
  unsigned int result = clang_PrintingPolicy_getProperty(Policy, Property);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_PrintingPolicy_setProperty_wrapper(value Policy_ocaml, value Property_ocaml, value Value_ocaml)
{
  CAMLparam3(Policy_ocaml, Property_ocaml, Value_ocaml);
  CXPrintingPolicy Policy;
  Policy = Cxprintingpolicy_val(Policy_ocaml);
  enum CXPrintingPolicyProperty Property;
  Property = Cxprintingpolicyproperty_val(Property_ocaml);
  unsigned int Value;
  Value = Int_val(Value_ocaml);
  clang_PrintingPolicy_setProperty(Policy, Property, Value);
  CAMLreturn(Val_unit);
}

CAMLprim value
clang_getCursorPrintingPolicy_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXCursor arg;
  arg = Cxcursor_val(Field(arg_ocaml, 0));
  CXPrintingPolicy result = clang_getCursorPrintingPolicy(arg);
  {
    CAMLlocal1(data);
    data = Val_cxprintingpolicy(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCursorPrettyPrinted_wrapper(value Cursor_ocaml, value Policy_ocaml)
{
  CAMLparam2(Cursor_ocaml, Policy_ocaml);
  CXCursor Cursor;
  Cursor = Cxcursor_val(Field(Cursor_ocaml, 0));
  CXPrintingPolicy Policy;
  Policy = Cxprintingpolicy_val(Policy_ocaml);
  CXString result = clang_getCursorPrettyPrinted(Cursor, Policy);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCursorDisplayName_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXCursor arg;
  arg = Cxcursor_val(Field(arg_ocaml, 0));
  CXString result = clang_getCursorDisplayName(arg);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCursorReferenced_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXCursor arg;
  arg = Cxcursor_val(Field(arg_ocaml, 0));
  CXCursor result = clang_getCursorReferenced(arg);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxcursor(result));
  Store_field(data, 1, Field(arg_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCursorDefinition_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXCursor arg;
  arg = Cxcursor_val(Field(arg_ocaml, 0));
  CXCursor result = clang_getCursorDefinition(arg);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxcursor(result));
  Store_field(data, 1, Field(arg_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_isCursorDefinition_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXCursor arg;
  arg = Cxcursor_val(Field(arg_ocaml, 0));
  unsigned int result = clang_isCursorDefinition(arg);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCanonicalCursor_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXCursor arg;
  arg = Cxcursor_val(Field(arg_ocaml, 0));
  CXCursor result = clang_getCanonicalCursor(arg);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxcursor(result));
  Store_field(data, 1, Field(arg_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_getObjCSelectorIndex_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXCursor arg;
  arg = Cxcursor_val(Field(arg_ocaml, 0));
  int result = clang_Cursor_getObjCSelectorIndex(arg);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_isDynamicCall_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  int result = clang_Cursor_isDynamicCall(C);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_getReceiverType_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  CXType result = clang_Cursor_getReceiverType(C);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxtype(result));
  Store_field(data, 1, Field(C_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_getObjCPropertyAttributes_wrapper(value C_ocaml, value reserved_ocaml)
{
  CAMLparam2(C_ocaml, reserved_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int reserved;
  reserved = Int_val(reserved_ocaml);
  unsigned int result = clang_Cursor_getObjCPropertyAttributes(C, reserved);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_getObjCDeclQualifiers_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int result = clang_Cursor_getObjCDeclQualifiers(C);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_isObjCOptional_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int result = clang_Cursor_isObjCOptional(C);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_isVariadic_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int result = clang_Cursor_isVariadic(C);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_isExternalSymbol_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  CXString language;
  CXString definedIn;
  unsigned int isGenerated;
  unsigned int result = clang_Cursor_isExternalSymbol(C, &language, &definedIn, &isGenerated);
  if (result) {
    CAMLlocal2(ocaml_result, data);
    ocaml_result = caml_alloc(1, 0);
    data = caml_alloc_tuple(3);
  {
    CAMLlocal1(field);
    field = caml_copy_string(clang_getCString(language));
                    clang_disposeString(language);
    Store_field(data, 0, field);
  }
  {
    CAMLlocal1(field);
    field = caml_copy_string(clang_getCString(definedIn));
                    clang_disposeString(definedIn);
    Store_field(data, 1, field);
  }
  {
    CAMLlocal1(field);
    field = Val_int(isGenerated);
    Store_field(data, 2, field);
  }

    Store_field(ocaml_result, 0, data);
    
    CAMLreturn(ocaml_result);
  }
  else {
    CAMLreturn(Val_int(0));
  }}

CAMLprim value
clang_Cursor_getCommentRange_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  CXSourceRange result = clang_Cursor_getCommentRange(C);
  {
    CAMLlocal1(data);
    data = Val_cxsourcerange(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_getRawCommentText_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  CXString result = clang_Cursor_getRawCommentText(C);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_getBriefCommentText_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  CXString result = clang_Cursor_getBriefCommentText(C);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_getMangling_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXCursor arg;
  arg = Cxcursor_val(Field(arg_ocaml, 0));
  CXString result = clang_Cursor_getMangling(arg);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_getCXXManglings_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXCursor arg;
  arg = Cxcursor_val(Field(arg_ocaml, 0));
  CXStringSet * result = clang_Cursor_getCXXManglings(arg);
  {
    CAMLlocal1(data);
    
data = caml_alloc(result->Count, 0);
for (unsigned int i = 0; i < result->Count; i++) {
  CAMLlocal1(field);
  field = caml_copy_string(clang_getCString(result->Strings[i]));
                    clang_disposeString(result->Strings[i]);
  Store_field(data, i, field);
}
clang_disposeStringSet(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Cursor_getObjCManglings_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXCursor arg;
  arg = Cxcursor_val(Field(arg_ocaml, 0));
  CXStringSet * result = clang_Cursor_getObjCManglings(arg);
  {
    CAMLlocal1(data);
    
data = caml_alloc(result->Count, 0);
for (unsigned int i = 0; i < result->Count; i++) {
  CAMLlocal1(field);
  field = caml_copy_string(clang_getCString(result->Strings[i]));
                    clang_disposeString(result->Strings[i]);
  Store_field(data, i, field);
}
clang_disposeStringSet(result);
    CAMLreturn(data);
  }
}

DECLARE_OPAQUE(CXModule, cxmodule, Cxmodule_val, Val_cxmodule, custom_finalize_default)

CAMLprim value
clang_Cursor_getModule_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  CXModule result = clang_Cursor_getModule(C);
  {
    CAMLlocal1(data);
    data = Val_cxmodule(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getModuleForFile_wrapper(value arg_ocaml, value arg2_ocaml)
{
  CAMLparam2(arg_ocaml, arg2_ocaml);
  CXTranslationUnit arg;
  arg = Cxtranslationunit_val(Field(arg_ocaml, 0));
  CXFile arg2;
  arg2 = Cxfile_val(arg2_ocaml);
  CXModule result = clang_getModuleForFile(arg, arg2);
  {
    CAMLlocal1(data);
    data = Val_cxmodule(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Module_getASTFile_wrapper(value Module_ocaml)
{
  CAMLparam1(Module_ocaml);
  CXModule Module;
  Module = Cxmodule_val(Module_ocaml);
  CXFile result = clang_Module_getASTFile(Module);
  {
    CAMLlocal1(data);
    data = Val_cxfile(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Module_getParent_wrapper(value Module_ocaml)
{
  CAMLparam1(Module_ocaml);
  CXModule Module;
  Module = Cxmodule_val(Module_ocaml);
  CXModule result = clang_Module_getParent(Module);
  {
    CAMLlocal1(data);
    data = Val_cxmodule(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Module_getName_wrapper(value Module_ocaml)
{
  CAMLparam1(Module_ocaml);
  CXModule Module;
  Module = Cxmodule_val(Module_ocaml);
  CXString result = clang_Module_getName(Module);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Module_getFullName_wrapper(value Module_ocaml)
{
  CAMLparam1(Module_ocaml);
  CXModule Module;
  Module = Cxmodule_val(Module_ocaml);
  CXString result = clang_Module_getFullName(Module);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Module_isSystem_wrapper(value Module_ocaml)
{
  CAMLparam1(Module_ocaml);
  CXModule Module;
  Module = Cxmodule_val(Module_ocaml);
  int result = clang_Module_isSystem(Module);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Module_getNumTopLevelHeaders_wrapper(value arg_ocaml, value Module_ocaml)
{
  CAMLparam2(arg_ocaml, Module_ocaml);
  CXTranslationUnit arg;
  arg = Cxtranslationunit_val(Field(arg_ocaml, 0));
  CXModule Module;
  Module = Cxmodule_val(Module_ocaml);
  unsigned int result = clang_Module_getNumTopLevelHeaders(arg, Module);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_Module_getTopLevelHeader_wrapper(value arg_ocaml, value Module_ocaml, value Index_ocaml)
{
  CAMLparam3(arg_ocaml, Module_ocaml, Index_ocaml);
  CXTranslationUnit arg;
  arg = Cxtranslationunit_val(Field(arg_ocaml, 0));
  CXModule Module;
  Module = Cxmodule_val(Module_ocaml);
  unsigned int Index;
  Index = Int_val(Index_ocaml);
  CXFile result = clang_Module_getTopLevelHeader(arg, Module, Index);
  {
    CAMLlocal1(data);
    data = Val_cxfile(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_CXXConstructor_isConvertingConstructor_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int result = clang_CXXConstructor_isConvertingConstructor(C);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_CXXConstructor_isCopyConstructor_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int result = clang_CXXConstructor_isCopyConstructor(C);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_CXXConstructor_isDefaultConstructor_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int result = clang_CXXConstructor_isDefaultConstructor(C);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_CXXConstructor_isMoveConstructor_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int result = clang_CXXConstructor_isMoveConstructor(C);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_CXXField_isMutable_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int result = clang_CXXField_isMutable(C);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_CXXMethod_isDefaulted_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int result = clang_CXXMethod_isDefaulted(C);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_CXXMethod_isPureVirtual_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int result = clang_CXXMethod_isPureVirtual(C);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_CXXMethod_isStatic_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int result = clang_CXXMethod_isStatic(C);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_CXXMethod_isVirtual_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int result = clang_CXXMethod_isVirtual(C);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_CXXRecord_isAbstract_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int result = clang_CXXRecord_isAbstract(C);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_EnumDecl_isScoped_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int result = clang_EnumDecl_isScoped(C);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_CXXMethod_isConst_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int result = clang_CXXMethod_isConst(C);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getTemplateCursorKind_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  enum CXCursorKind result = clang_getTemplateCursorKind(C);
  {
    CAMLlocal1(data);
    data = Val_cxcursorkind(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getSpecializedCursorTemplate_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  CXCursor result = clang_getSpecializedCursorTemplate(C);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxcursor(result));
  Store_field(data, 1, Field(C_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCursorReferenceNameRange_wrapper(value C_ocaml, value NameFlags_ocaml, value PieceIndex_ocaml)
{
  CAMLparam3(C_ocaml, NameFlags_ocaml, PieceIndex_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  unsigned int NameFlags;
  NameFlags = Int_val(NameFlags_ocaml);
  unsigned int PieceIndex;
  PieceIndex = Int_val(PieceIndex_ocaml);
  CXSourceRange result = clang_getCursorReferenceNameRange(C, NameFlags, PieceIndex);
  {
    CAMLlocal1(data);
    data = Val_cxsourcerange(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCursorKindSpelling_wrapper(value Kind_ocaml)
{
  CAMLparam1(Kind_ocaml);
  enum CXCursorKind Kind;
  Kind = Cxcursorkind_val(Kind_ocaml);
  CXString result = clang_getCursorKindSpelling(Kind);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_enableStackTraces_wrapper()
{
  CAMLparam0();
  clang_enableStackTraces();
  CAMLreturn(Val_unit);
}

enum CXCompletionChunkKind
Cxcompletionchunkkind_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return CXCompletionChunk_Optional;
  case 1: return CXCompletionChunk_TypedText;
  case 2: return CXCompletionChunk_Text;
  case 3: return CXCompletionChunk_Placeholder;
  case 4: return CXCompletionChunk_Informative;
  case 5: return CXCompletionChunk_CurrentParameter;
  case 6: return CXCompletionChunk_LeftParen;
  case 7: return CXCompletionChunk_RightParen;
  case 8: return CXCompletionChunk_LeftBracket;
  case 9: return CXCompletionChunk_RightBracket;
  case 10: return CXCompletionChunk_LeftBrace;
  case 11: return CXCompletionChunk_RightBrace;
  case 12: return CXCompletionChunk_LeftAngle;
  case 13: return CXCompletionChunk_RightAngle;
  case 14: return CXCompletionChunk_Comma;
  case 15: return CXCompletionChunk_ResultType;
  case 16: return CXCompletionChunk_Colon;
  case 17: return CXCompletionChunk_SemiColon;
  case 18: return CXCompletionChunk_Equal;
  case 19: return CXCompletionChunk_HorizontalSpace;
  case 20: return CXCompletionChunk_VerticalSpace;
  }
  failwith_fmt("invalid value for Cxcompletionchunkkind_val: %d", Int_val(ocaml));
  return CXCompletionChunk_Optional;
}

value
Val_cxcompletionchunkkind(enum CXCompletionChunkKind v)
{
  switch (v) {
  case CXCompletionChunk_Optional: return Val_int(0);
  case CXCompletionChunk_TypedText: return Val_int(1);
  case CXCompletionChunk_Text: return Val_int(2);
  case CXCompletionChunk_Placeholder: return Val_int(3);
  case CXCompletionChunk_Informative: return Val_int(4);
  case CXCompletionChunk_CurrentParameter: return Val_int(5);
  case CXCompletionChunk_LeftParen: return Val_int(6);
  case CXCompletionChunk_RightParen: return Val_int(7);
  case CXCompletionChunk_LeftBracket: return Val_int(8);
  case CXCompletionChunk_RightBracket: return Val_int(9);
  case CXCompletionChunk_LeftBrace: return Val_int(10);
  case CXCompletionChunk_RightBrace: return Val_int(11);
  case CXCompletionChunk_LeftAngle: return Val_int(12);
  case CXCompletionChunk_RightAngle: return Val_int(13);
  case CXCompletionChunk_Comma: return Val_int(14);
  case CXCompletionChunk_ResultType: return Val_int(15);
  case CXCompletionChunk_Colon: return Val_int(16);
  case CXCompletionChunk_SemiColon: return Val_int(17);
  case CXCompletionChunk_Equal: return Val_int(18);
  case CXCompletionChunk_HorizontalSpace: return Val_int(19);
  case CXCompletionChunk_VerticalSpace: return Val_int(20);
  }
  failwith_fmt("invalid value for Val_cxcompletionchunkkind: %d", v);
  return Val_int(0);
}

DECLARE_OPAQUE(CXCompletionString, cxcompletionstring, Cxcompletionstring_val, Val_cxcompletionstring, custom_finalize_default)

CAMLprim value
clang_getCompletionChunkKind_wrapper(value completion_string_ocaml, value chunk_number_ocaml)
{
  CAMLparam2(completion_string_ocaml, chunk_number_ocaml);
  CXCompletionString completion_string;
  completion_string = Cxcompletionstring_val(completion_string_ocaml);
  unsigned int chunk_number;
  chunk_number = Int_val(chunk_number_ocaml);
  enum CXCompletionChunkKind result = clang_getCompletionChunkKind(completion_string, chunk_number);
  {
    CAMLlocal1(data);
    data = Val_cxcompletionchunkkind(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCompletionChunkText_wrapper(value completion_string_ocaml, value chunk_number_ocaml)
{
  CAMLparam2(completion_string_ocaml, chunk_number_ocaml);
  CXCompletionString completion_string;
  completion_string = Cxcompletionstring_val(completion_string_ocaml);
  unsigned int chunk_number;
  chunk_number = Int_val(chunk_number_ocaml);
  CXString result = clang_getCompletionChunkText(completion_string, chunk_number);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCompletionChunkCompletionString_wrapper(value completion_string_ocaml, value chunk_number_ocaml)
{
  CAMLparam2(completion_string_ocaml, chunk_number_ocaml);
  CXCompletionString completion_string;
  completion_string = Cxcompletionstring_val(completion_string_ocaml);
  unsigned int chunk_number;
  chunk_number = Int_val(chunk_number_ocaml);
  CXCompletionString result = clang_getCompletionChunkCompletionString(completion_string, chunk_number);
  {
    CAMLlocal1(data);
    data = Val_cxcompletionstring(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getNumCompletionChunks_wrapper(value completion_string_ocaml)
{
  CAMLparam1(completion_string_ocaml);
  CXCompletionString completion_string;
  completion_string = Cxcompletionstring_val(completion_string_ocaml);
  unsigned int result = clang_getNumCompletionChunks(completion_string);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCompletionPriority_wrapper(value completion_string_ocaml)
{
  CAMLparam1(completion_string_ocaml);
  CXCompletionString completion_string;
  completion_string = Cxcompletionstring_val(completion_string_ocaml);
  unsigned int result = clang_getCompletionPriority(completion_string);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCompletionAvailability_wrapper(value completion_string_ocaml)
{
  CAMLparam1(completion_string_ocaml);
  CXCompletionString completion_string;
  completion_string = Cxcompletionstring_val(completion_string_ocaml);
  enum CXAvailabilityKind result = clang_getCompletionAvailability(completion_string);
  {
    CAMLlocal1(data);
    data = Val_cxavailabilitykind(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCompletionNumAnnotations_wrapper(value completion_string_ocaml)
{
  CAMLparam1(completion_string_ocaml);
  CXCompletionString completion_string;
  completion_string = Cxcompletionstring_val(completion_string_ocaml);
  unsigned int result = clang_getCompletionNumAnnotations(completion_string);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCompletionAnnotation_wrapper(value completion_string_ocaml, value annotation_number_ocaml)
{
  CAMLparam2(completion_string_ocaml, annotation_number_ocaml);
  CXCompletionString completion_string;
  completion_string = Cxcompletionstring_val(completion_string_ocaml);
  unsigned int annotation_number;
  annotation_number = Int_val(annotation_number_ocaml);
  CXString result = clang_getCompletionAnnotation(completion_string, annotation_number);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCompletionParent_wrapper(value completion_string_ocaml)
{
  CAMLparam1(completion_string_ocaml);
  CXCompletionString completion_string;
  completion_string = Cxcompletionstring_val(completion_string_ocaml);
  CXString result = clang_getCompletionParent(completion_string, NULL);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCompletionBriefComment_wrapper(value completion_string_ocaml)
{
  CAMLparam1(completion_string_ocaml);
  CXCompletionString completion_string;
  completion_string = Cxcompletionstring_val(completion_string_ocaml);
  CXString result = clang_getCompletionBriefComment(completion_string);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getCursorCompletionString_wrapper(value cursor_ocaml)
{
  CAMLparam1(cursor_ocaml);
  CXCursor cursor;
  cursor = Cxcursor_val(Field(cursor_ocaml, 0));
  CXCompletionString result = clang_getCursorCompletionString(cursor);
  {
    CAMLlocal1(data);
    data = Val_cxcompletionstring(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_defaultCodeCompleteOptions_wrapper()
{
  CAMLparam0();
  unsigned int result = clang_defaultCodeCompleteOptions();
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getClangVersion_wrapper()
{
  CAMLparam0();
  CXString result = clang_getClangVersion();
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_toggleCrashRecovery_wrapper(value isEnabled_ocaml)
{
  CAMLparam1(isEnabled_ocaml);
  unsigned int isEnabled;
  isEnabled = Int_val(isEnabled_ocaml);
  clang_toggleCrashRecovery(isEnabled);
  CAMLreturn(Val_unit);
}

DECLARE_OPAQUE(CXEvalResult, cxevalresult, Cxevalresult_val, Val_cxevalresult, custom_finalize_default)

CAMLprim value
clang_Cursor_Evaluate_wrapper(value C_ocaml)
{
  CAMLparam1(C_ocaml);
  CXCursor C;
  C = Cxcursor_val(Field(C_ocaml, 0));
  CXEvalResult result = clang_Cursor_Evaluate(C);
  {
    CAMLlocal1(data);
    data = Val_cxevalresult(result);
    CAMLreturn(data);
  }
}

CXEvalResultKind
Cxevalresultkind_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return CXEval_Int;
  case 1: return CXEval_Float;
  case 2: return CXEval_ObjCStrLiteral;
  case 3: return CXEval_StrLiteral;
  case 4: return CXEval_CFStr;
  case 5: return CXEval_Other;
  case 6: return CXEval_UnExposed;
  }
  failwith_fmt("invalid value for Cxevalresultkind_val: %d", Int_val(ocaml));
  return CXEval_Int;
}

value
Val_cxevalresultkind(CXEvalResultKind v)
{
  switch (v) {
  case CXEval_Int: return Val_int(0);
  case CXEval_Float: return Val_int(1);
  case CXEval_ObjCStrLiteral: return Val_int(2);
  case CXEval_StrLiteral: return Val_int(3);
  case CXEval_CFStr: return Val_int(4);
  case CXEval_Other: return Val_int(5);
  case CXEval_UnExposed: return Val_int(6);
  }
  failwith_fmt("invalid value for Val_cxevalresultkind: %d", v);
  return Val_int(0);
}

CAMLprim value
clang_EvalResult_getKind_wrapper(value E_ocaml)
{
  CAMLparam1(E_ocaml);
  CXEvalResult E;
  E = Cxevalresult_val(E_ocaml);
  CXEvalResultKind result = clang_EvalResult_getKind(E);
  {
    CAMLlocal1(data);
    data = Val_cxevalresultkind(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_EvalResult_getAsInt_wrapper(value E_ocaml)
{
  CAMLparam1(E_ocaml);
  CXEvalResult E;
  E = Cxevalresult_val(E_ocaml);
  int result = clang_EvalResult_getAsInt(E);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_EvalResult_getAsLongLong_wrapper(value E_ocaml)
{
  CAMLparam1(E_ocaml);
  CXEvalResult E;
  E = Cxevalresult_val(E_ocaml);
  long long result = clang_EvalResult_getAsLongLong(E);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_EvalResult_isUnsignedInt_wrapper(value E_ocaml)
{
  CAMLparam1(E_ocaml);
  CXEvalResult E;
  E = Cxevalresult_val(E_ocaml);
  unsigned int result = clang_EvalResult_isUnsignedInt(E);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_EvalResult_getAsUnsigned_wrapper(value E_ocaml)
{
  CAMLparam1(E_ocaml);
  CXEvalResult E;
  E = Cxevalresult_val(E_ocaml);
  unsigned long long result = clang_EvalResult_getAsUnsigned(E);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_EvalResult_getAsDouble_wrapper(value E_ocaml)
{
  CAMLparam1(E_ocaml);
  CXEvalResult E;
  E = Cxevalresult_val(E_ocaml);
  double result = clang_EvalResult_getAsDouble(E);
  {
    CAMLlocal1(data);
    data = caml_copy_double(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_EvalResult_getAsStr_wrapper(value E_ocaml)
{
  CAMLparam1(E_ocaml);
  CXEvalResult E;
  E = Cxevalresult_val(E_ocaml);
  const char * result = clang_EvalResult_getAsStr(E);
  {
    CAMLlocal1(data);
    data = caml_copy_string(result);
    CAMLreturn(data);
  }
}

DECLARE_OPAQUE(CXRemapping, cxremapping, Cxremapping_val, Val_cxremapping, custom_finalize_default)

CAMLprim value
clang_getRemappings_wrapper(value path_ocaml)
{
  CAMLparam1(path_ocaml);
  const char * path;
  path = String_val(path_ocaml);
  CXRemapping result = clang_getRemappings(path);
  {
    CAMLlocal1(data);
    data = Val_cxremapping(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_getRemappingsFromFileList_wrapper(value filePaths_ocaml)
{
  CAMLparam1(filePaths_ocaml);
  unsigned int numFiles = Wosize_val(filePaths_ocaml);
   char ** filePaths = xmalloc(numFiles * sizeof(const char *));
  unsigned int i; for (i = 0; i < numFiles; i++) {
    filePaths[i] = String_val(Field(filePaths_ocaml, i));
  }
  CXRemapping result = clang_getRemappingsFromFileList((const char **) filePaths, numFiles);
  {
    CAMLlocal1(data);
    data = Val_cxremapping(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_remap_getNumFiles_wrapper(value arg_ocaml)
{
  CAMLparam1(arg_ocaml);
  CXRemapping arg;
  arg = Cxremapping_val(arg_ocaml);
  unsigned int result = clang_remap_getNumFiles(arg);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

DECLARE_OPAQUE(CXIndexAction, cxindexaction, Cxindexaction_val, Val_cxindexaction, custom_finalize_default)

CAMLprim value
clang_IndexAction_create_wrapper(value CIdx_ocaml)
{
  CAMLparam1(CIdx_ocaml);
  CXIndex CIdx;
  CIdx = Cxindex_val(CIdx_ocaml);
  CXIndexAction result = clang_IndexAction_create(CIdx);
  {
    CAMLlocal1(data);
    data = Val_cxindexaction(result);
    CAMLreturn(data);
  }
}

enum CXVisitorResult
Cxvisitorresult_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return CXVisit_Break;
  case 1: return CXVisit_Continue;
  }
  failwith_fmt("invalid value for Cxvisitorresult_val: %d", Int_val(ocaml));
  return CXVisit_Break;
}

value
Val_cxvisitorresult(enum CXVisitorResult v)
{
  switch (v) {
  case CXVisit_Break: return Val_int(0);
  case CXVisit_Continue: return Val_int(1);
  }
  failwith_fmt("invalid value for Val_cxvisitorresult: %d", v);
  return Val_int(0);
}

enum CXVisitorResult
clang_Type_visitFields_visitor_callback(CXCursor arg0, CXClientData arg1)
{
  CAMLparam0();
  CAMLlocal3(result, f, arg0_ocaml);
  f = *((value *) ((value **)arg1)[0]);
arg0_ocaml = caml_alloc_tuple(2);
  Store_field(arg0_ocaml, 0, Val_cxcursor(arg0));
  Store_field(arg0_ocaml, 1, *((value **)arg1)[1]);  result = caml_callback(f, arg0_ocaml);
  {
    CAMLlocal1(data);
    data = Cxvisitorresult_val(result);
    CAMLreturnT(enum CXVisitorResult, data);
  }

}

CAMLprim value
clang_Type_visitFields_wrapper(value T_ocaml, value visitor_ocaml)
{
  CAMLparam2(T_ocaml, visitor_ocaml);
  CXType T;
  T = Cxtype_val(Field(T_ocaml, 0));
  unsigned int result = clang_Type_visitFields(T, clang_Type_visitFields_visitor_callback, (value *[]){&visitor_ocaml,&T_ocaml});
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

static void finalize_cxint(value v) {
  clang_ext_Int_dispose(*((CXInt *) Data_custom_val(v)));;
}
DECLARE_OPAQUE(CXInt, cxint, Cxint_val, Val_cxint, finalize_cxint)

CAMLprim value
clang_equal_cxint_wrapper(value a_ocaml, value b_ocaml)
{
  CAMLparam2(a_ocaml, b_ocaml);
  CXInt a;
  a = Cxint_val(a_ocaml);
  CXInt b;
  b = Cxint_val(b_ocaml);
  _Bool result = clang_equal_cxint(a, b);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_compare_cxint_wrapper(value a_ocaml, value b_ocaml)
{
  CAMLparam2(a_ocaml, b_ocaml);
  CXInt a;
  a = Cxint_val(a_ocaml);
  CXInt b;
  b = Cxint_val(b_ocaml);
  int result = clang_compare_cxint(a, b);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_IntegerLiteral_getValue_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXCursor c;
  c = Cxcursor_val(Field(c_ocaml, 0));
  CXInt result = clang_ext_IntegerLiteral_getValue(c);
  {
    CAMLlocal1(data);
    data = Val_cxint(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_Int_isValid_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXInt c;
  c = Cxint_val(c_ocaml);
  _Bool result = clang_ext_Int_isValid(c);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_Int_toString_wrapper(value c_ocaml, value Radix_ocaml, value isSigned_ocaml)
{
  CAMLparam3(c_ocaml, Radix_ocaml, isSigned_ocaml);
  CXInt c;
  c = Cxint_val(c_ocaml);
  unsigned int Radix;
  Radix = Int_val(Radix_ocaml);
  _Bool isSigned;
  isSigned = Bool_val(isSigned_ocaml);
  CXString result = clang_ext_Int_toString(c, Radix, isSigned);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_Int_roundToDouble_wrapper(value c_ocaml, value isSigned_ocaml)
{
  CAMLparam2(c_ocaml, isSigned_ocaml);
  CXInt c;
  c = Cxint_val(c_ocaml);
  _Bool isSigned;
  isSigned = Bool_val(isSigned_ocaml);
  double result = clang_ext_Int_roundToDouble(c, isSigned);
  {
    CAMLlocal1(data);
    data = caml_copy_double(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_Int_bitsToFloat_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXInt c;
  c = Cxint_val(c_ocaml);
  float result = clang_ext_Int_bitsToFloat(c);
  {
    CAMLlocal1(data);
    data = caml_copy_double(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_Int_getBitWidth_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXInt c;
  c = Cxint_val(c_ocaml);
  unsigned int result = clang_ext_Int_getBitWidth(c);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_Int_getActiveBits_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXInt c;
  c = Cxint_val(c_ocaml);
  unsigned int result = clang_ext_Int_getActiveBits(c);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_Int_getMinSignedBits_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXInt c;
  c = Cxint_val(c_ocaml);
  unsigned int result = clang_ext_Int_getMinSignedBits(c);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_Int_getBoolValue_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXInt c;
  c = Cxint_val(c_ocaml);
  _Bool result = clang_ext_Int_getBoolValue(c);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_Int_getSExtValue_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXInt c;
  c = Cxint_val(c_ocaml);
  int result = clang_ext_Int_getSExtValue(c);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_Int_getSExtValue64_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXInt c;
  c = Cxint_val(c_ocaml);
  int64_t result = clang_ext_Int_getSExtValue64(c);
  {
    CAMLlocal1(data);
    data = copy_int64(result);
    CAMLreturn(data);
  }
}

static void finalize_cxfloat(value v) {
  clang_ext_Float_dispose(*((CXFloat *) Data_custom_val(v)));;
}
DECLARE_OPAQUE(CXFloat, cxfloat, Cxfloat_val, Val_cxfloat, finalize_cxfloat)

CAMLprim value
clang_equal_cxfloat_wrapper(value a_ocaml, value b_ocaml)
{
  CAMLparam2(a_ocaml, b_ocaml);
  CXFloat a;
  a = Cxfloat_val(a_ocaml);
  CXFloat b;
  b = Cxfloat_val(b_ocaml);
  _Bool result = clang_equal_cxfloat(a, b);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_compare_cxfloat_wrapper(value a_ocaml, value b_ocaml)
{
  CAMLparam2(a_ocaml, b_ocaml);
  CXFloat a;
  a = Cxfloat_val(a_ocaml);
  CXFloat b;
  b = Cxfloat_val(b_ocaml);
  int result = clang_compare_cxfloat(a, b);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_FloatingLiteral_getValue_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXCursor c;
  c = Cxcursor_val(Field(c_ocaml, 0));
  CXFloat result = clang_ext_FloatingLiteral_getValue(c);
  {
    CAMLlocal1(data);
    data = Val_cxfloat(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_Float_isValid_wrapper(value f_ocaml)
{
  CAMLparam1(f_ocaml);
  CXFloat f;
  f = Cxfloat_val(f_ocaml);
  _Bool result = clang_ext_Float_isValid(f);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_Float_toString_wrapper(value f_ocaml)
{
  CAMLparam1(f_ocaml);
  CXFloat f;
  f = Cxfloat_val(f_ocaml);
  CXString result = clang_ext_Float_toString(f);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_Float_convertToDouble_wrapper(value f_ocaml)
{
  CAMLparam1(f_ocaml);
  CXFloat f;
  f = Cxfloat_val(f_ocaml);
  double result = clang_ext_Float_convertToDouble(f);
  {
    CAMLlocal1(data);
    data = caml_copy_double(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_StringLiteral_GetString_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXCursor c;
  c = Cxcursor_val(Field(c_ocaml, 0));
  CXString result = clang_ext_StringLiteral_GetString(c);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

enum clang_ext_UnaryOperatorKind
Clang_ext_unaryoperatorkind_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return CLANG_EXT_UNARY_OPERATOR_PostInc;
  case 1: return CLANG_EXT_UNARY_OPERATOR_PostDec;
  case 2: return CLANG_EXT_UNARY_OPERATOR_PreInc;
  case 3: return CLANG_EXT_UNARY_OPERATOR_PreDec;
  case 4: return CLANG_EXT_UNARY_OPERATOR_AddrOf;
  case 5: return CLANG_EXT_UNARY_OPERATOR_Deref;
  case 6: return CLANG_EXT_UNARY_OPERATOR_Plus;
  case 7: return CLANG_EXT_UNARY_OPERATOR_Minus;
  case 8: return CLANG_EXT_UNARY_OPERATOR_Not;
  case 9: return CLANG_EXT_UNARY_OPERATOR_LNot;
  case 10: return CLANG_EXT_UNARY_OPERATOR_Real;
  case 11: return CLANG_EXT_UNARY_OPERATOR_Imag;
  case 12: return CLANG_EXT_UNARY_OPERATOR_Extension;
  case 13: return CLANG_EXT_UNARY_OPERATOR_Coawait;
  }
  failwith_fmt("invalid value for Clang_ext_unaryoperatorkind_val: %d", Int_val(ocaml));
  return CLANG_EXT_UNARY_OPERATOR_PostInc;
}

value
Val_clang_ext_unaryoperatorkind(enum clang_ext_UnaryOperatorKind v)
{
  switch (v) {
  case CLANG_EXT_UNARY_OPERATOR_PostInc: return Val_int(0);
  case CLANG_EXT_UNARY_OPERATOR_PostDec: return Val_int(1);
  case CLANG_EXT_UNARY_OPERATOR_PreInc: return Val_int(2);
  case CLANG_EXT_UNARY_OPERATOR_PreDec: return Val_int(3);
  case CLANG_EXT_UNARY_OPERATOR_AddrOf: return Val_int(4);
  case CLANG_EXT_UNARY_OPERATOR_Deref: return Val_int(5);
  case CLANG_EXT_UNARY_OPERATOR_Plus: return Val_int(6);
  case CLANG_EXT_UNARY_OPERATOR_Minus: return Val_int(7);
  case CLANG_EXT_UNARY_OPERATOR_Not: return Val_int(8);
  case CLANG_EXT_UNARY_OPERATOR_LNot: return Val_int(9);
  case CLANG_EXT_UNARY_OPERATOR_Real: return Val_int(10);
  case CLANG_EXT_UNARY_OPERATOR_Imag: return Val_int(11);
  case CLANG_EXT_UNARY_OPERATOR_Extension: return Val_int(12);
  case CLANG_EXT_UNARY_OPERATOR_Coawait: return Val_int(13);
  }
  failwith_fmt("invalid value for Val_clang_ext_unaryoperatorkind: %d", v);
  return Val_int(0);
}

CAMLprim value
clang_ext_UnaryOperator_getOpcode_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXCursor c;
  c = Cxcursor_val(Field(c_ocaml, 0));
  enum clang_ext_UnaryOperatorKind result = clang_ext_UnaryOperator_getOpcode(c);
  {
    CAMLlocal1(data);
    data = Val_clang_ext_unaryoperatorkind(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_UnaryOperator_getOpcodeSpelling_wrapper(value Kind_ocaml)
{
  CAMLparam1(Kind_ocaml);
  enum clang_ext_UnaryOperatorKind Kind;
  Kind = Clang_ext_unaryoperatorkind_val(Kind_ocaml);
  CXString result = clang_ext_UnaryOperator_getOpcodeSpelling(Kind);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

enum clang_ext_BinaryOperatorKind
Clang_ext_binaryoperatorkind_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return CLANG_EXT_BINARY_OPERATOR_PtrMemD;
  case 1: return CLANG_EXT_BINARY_OPERATOR_PtrMemI;
  case 2: return CLANG_EXT_BINARY_OPERATOR_Mul;
  case 3: return CLANG_EXT_BINARY_OPERATOR_Div;
  case 4: return CLANG_EXT_BINARY_OPERATOR_Rem;
  case 5: return CLANG_EXT_BINARY_OPERATOR_Add;
  case 6: return CLANG_EXT_BINARY_OPERATOR_Sub;
  case 7: return CLANG_EXT_BINARY_OPERATOR_Shl;
  case 8: return CLANG_EXT_BINARY_OPERATOR_Shr;
  case 9: return CLANG_EXT_BINARY_OPERATOR_Cmp;
  case 10: return CLANG_EXT_BINARY_OPERATOR_LT;
  case 11: return CLANG_EXT_BINARY_OPERATOR_GT;
  case 12: return CLANG_EXT_BINARY_OPERATOR_LE;
  case 13: return CLANG_EXT_BINARY_OPERATOR_GE;
  case 14: return CLANG_EXT_BINARY_OPERATOR_EQ;
  case 15: return CLANG_EXT_BINARY_OPERATOR_NE;
  case 16: return CLANG_EXT_BINARY_OPERATOR_And;
  case 17: return CLANG_EXT_BINARY_OPERATOR_Xor;
  case 18: return CLANG_EXT_BINARY_OPERATOR_Or;
  case 19: return CLANG_EXT_BINARY_OPERATOR_LAnd;
  case 20: return CLANG_EXT_BINARY_OPERATOR_LOr;
  case 21: return CLANG_EXT_BINARY_OPERATOR_Assign;
  case 22: return CLANG_EXT_BINARY_OPERATOR_MulAssign;
  case 23: return CLANG_EXT_BINARY_OPERATOR_DivAssign;
  case 24: return CLANG_EXT_BINARY_OPERATOR_RemAssign;
  case 25: return CLANG_EXT_BINARY_OPERATOR_AddAssign;
  case 26: return CLANG_EXT_BINARY_OPERATOR_SubAssign;
  case 27: return CLANG_EXT_BINARY_OPERATOR_ShlAssign;
  case 28: return CLANG_EXT_BINARY_OPERATOR_ShrAssign;
  case 29: return CLANG_EXT_BINARY_OPERATOR_AndAssign;
  case 30: return CLANG_EXT_BINARY_OPERATOR_XorAssign;
  case 31: return CLANG_EXT_BINARY_OPERATOR_OrAssign;
  case 32: return CLANG_EXT_BINARY_OPERATOR_Comma;
  }
  failwith_fmt("invalid value for Clang_ext_binaryoperatorkind_val: %d", Int_val(ocaml));
  return CLANG_EXT_BINARY_OPERATOR_PtrMemD;
}

value
Val_clang_ext_binaryoperatorkind(enum clang_ext_BinaryOperatorKind v)
{
  switch (v) {
  case CLANG_EXT_BINARY_OPERATOR_PtrMemD: return Val_int(0);
  case CLANG_EXT_BINARY_OPERATOR_PtrMemI: return Val_int(1);
  case CLANG_EXT_BINARY_OPERATOR_Mul: return Val_int(2);
  case CLANG_EXT_BINARY_OPERATOR_Div: return Val_int(3);
  case CLANG_EXT_BINARY_OPERATOR_Rem: return Val_int(4);
  case CLANG_EXT_BINARY_OPERATOR_Add: return Val_int(5);
  case CLANG_EXT_BINARY_OPERATOR_Sub: return Val_int(6);
  case CLANG_EXT_BINARY_OPERATOR_Shl: return Val_int(7);
  case CLANG_EXT_BINARY_OPERATOR_Shr: return Val_int(8);
  case CLANG_EXT_BINARY_OPERATOR_Cmp: return Val_int(9);
  case CLANG_EXT_BINARY_OPERATOR_LT: return Val_int(10);
  case CLANG_EXT_BINARY_OPERATOR_GT: return Val_int(11);
  case CLANG_EXT_BINARY_OPERATOR_LE: return Val_int(12);
  case CLANG_EXT_BINARY_OPERATOR_GE: return Val_int(13);
  case CLANG_EXT_BINARY_OPERATOR_EQ: return Val_int(14);
  case CLANG_EXT_BINARY_OPERATOR_NE: return Val_int(15);
  case CLANG_EXT_BINARY_OPERATOR_And: return Val_int(16);
  case CLANG_EXT_BINARY_OPERATOR_Xor: return Val_int(17);
  case CLANG_EXT_BINARY_OPERATOR_Or: return Val_int(18);
  case CLANG_EXT_BINARY_OPERATOR_LAnd: return Val_int(19);
  case CLANG_EXT_BINARY_OPERATOR_LOr: return Val_int(20);
  case CLANG_EXT_BINARY_OPERATOR_Assign: return Val_int(21);
  case CLANG_EXT_BINARY_OPERATOR_MulAssign: return Val_int(22);
  case CLANG_EXT_BINARY_OPERATOR_DivAssign: return Val_int(23);
  case CLANG_EXT_BINARY_OPERATOR_RemAssign: return Val_int(24);
  case CLANG_EXT_BINARY_OPERATOR_AddAssign: return Val_int(25);
  case CLANG_EXT_BINARY_OPERATOR_SubAssign: return Val_int(26);
  case CLANG_EXT_BINARY_OPERATOR_ShlAssign: return Val_int(27);
  case CLANG_EXT_BINARY_OPERATOR_ShrAssign: return Val_int(28);
  case CLANG_EXT_BINARY_OPERATOR_AndAssign: return Val_int(29);
  case CLANG_EXT_BINARY_OPERATOR_XorAssign: return Val_int(30);
  case CLANG_EXT_BINARY_OPERATOR_OrAssign: return Val_int(31);
  case CLANG_EXT_BINARY_OPERATOR_Comma: return Val_int(32);
  }
  failwith_fmt("invalid value for Val_clang_ext_binaryoperatorkind: %d", v);
  return Val_int(0);
}

CAMLprim value
clang_ext_BinaryOperator_getOpcode_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXCursor c;
  c = Cxcursor_val(Field(c_ocaml, 0));
  enum clang_ext_BinaryOperatorKind result = clang_ext_BinaryOperator_getOpcode(c);
  {
    CAMLlocal1(data);
    data = Val_clang_ext_binaryoperatorkind(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_BinaryOperator_getOpcodeSpelling_wrapper(value Kind_ocaml)
{
  CAMLparam1(Kind_ocaml);
  enum clang_ext_BinaryOperatorKind Kind;
  Kind = Clang_ext_binaryoperatorkind_val(Kind_ocaml);
  CXString result = clang_ext_BinaryOperator_getOpcodeSpelling(Kind);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_ForStmt_getChildrenSet_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXCursor c;
  c = Cxcursor_val(Field(c_ocaml, 0));
  unsigned int result = clang_ext_ForStmt_getChildrenSet(c);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_IfStmt_getChildrenSet_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXCursor c;
  c = Cxcursor_val(Field(c_ocaml, 0));
  unsigned int result = clang_ext_IfStmt_getChildrenSet(c);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_IfStmt_getInit_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXCursor c;
  c = Cxcursor_val(Field(c_ocaml, 0));
  CXCursor result = clang_ext_IfStmt_getInit(c);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxcursor(result));
  Store_field(data, 1, Field(c_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_SwitchStmt_getChildrenSet_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXCursor c;
  c = Cxcursor_val(Field(c_ocaml, 0));
  unsigned int result = clang_ext_SwitchStmt_getChildrenSet(c);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_SwitchStmt_getInit_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXCursor c;
  c = Cxcursor_val(Field(c_ocaml, 0));
  CXCursor result = clang_ext_SwitchStmt_getInit(c);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxcursor(result));
  Store_field(data, 1, Field(c_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_WhileStmt_getChildrenSet_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXCursor c;
  c = Cxcursor_val(Field(c_ocaml, 0));
  unsigned int result = clang_ext_WhileStmt_getChildrenSet(c);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

enum clang_ext_ElaboratedTypeKeyword
Clang_ext_elaboratedtypekeyword_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return ETK_Struct;
  case 1: return ETK_Interface;
  case 2: return ETK_Union;
  case 3: return ETK_Class;
  case 4: return ETK_Enum;
  case 5: return ETK_Typename;
  case 6: return ETK_None;
  }
  failwith_fmt("invalid value for Clang_ext_elaboratedtypekeyword_val: %d", Int_val(ocaml));
  return ETK_Struct;
}

value
Val_clang_ext_elaboratedtypekeyword(enum clang_ext_ElaboratedTypeKeyword v)
{
  switch (v) {
  case ETK_Struct: return Val_int(0);
  case ETK_Interface: return Val_int(1);
  case ETK_Union: return Val_int(2);
  case ETK_Class: return Val_int(3);
  case ETK_Enum: return Val_int(4);
  case ETK_Typename: return Val_int(5);
  case ETK_None: return Val_int(6);
  }
  failwith_fmt("invalid value for Val_clang_ext_elaboratedtypekeyword: %d", v);
  return Val_int(0);
}

CAMLprim value
clang_ext_ElaboratedType_getKeyword_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXType c;
  c = Cxtype_val(Field(c_ocaml, 0));
  enum clang_ext_ElaboratedTypeKeyword result = clang_ext_ElaboratedType_getKeyword(c);
  {
    CAMLlocal1(data);
    data = Val_clang_ext_elaboratedtypekeyword(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_ElaboratedType_getKeywordSpelling_wrapper(value keyword_ocaml)
{
  CAMLparam1(keyword_ocaml);
  enum clang_ext_ElaboratedTypeKeyword keyword;
  keyword = Clang_ext_elaboratedtypekeyword_val(keyword_ocaml);
  CXString result = clang_ext_ElaboratedType_getKeywordSpelling(keyword);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_VarDecl_hasInit_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXCursor c;
  c = Cxcursor_val(Field(c_ocaml, 0));
  _Bool result = clang_ext_VarDecl_hasInit(c);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_MemberRefExpr_isArrow_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXCursor c;
  c = Cxcursor_val(Field(c_ocaml, 0));
  _Bool result = clang_ext_MemberRefExpr_isArrow(c);
  {
    CAMLlocal1(data);
    data = Val_bool(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_Stmt_GetClassName_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXCursor c;
  c = Cxcursor_val(Field(c_ocaml, 0));
  CXString result = clang_ext_Stmt_GetClassName(c);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_Stmt_GetClassKind_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXCursor c;
  c = Cxcursor_val(Field(c_ocaml, 0));
  int result = clang_ext_Stmt_GetClassKind(c);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

enum clang_ext_CursorKind
Clang_ext_cursorkind_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return ECK_ImplicitCastExpr;
  case 1: return ECK_BinaryConditionalOperator;
  case 2: return ECK_UnaryExprOrTypeTraitExpr;
  case 3: return ECK_Unknown;
  }
  failwith_fmt("invalid value for Clang_ext_cursorkind_val: %d", Int_val(ocaml));
  return ECK_ImplicitCastExpr;
}

value
Val_clang_ext_cursorkind(enum clang_ext_CursorKind v)
{
  switch (v) {
  case ECK_ImplicitCastExpr: return Val_int(0);
  case ECK_BinaryConditionalOperator: return Val_int(1);
  case ECK_UnaryExprOrTypeTraitExpr: return Val_int(2);
  case ECK_Unknown: return Val_int(3);
  }
  failwith_fmt("invalid value for Val_clang_ext_cursorkind: %d", v);
  return Val_int(0);
}

CAMLprim value
clang_ext_GetCursorKind_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXCursor c;
  c = Cxcursor_val(Field(c_ocaml, 0));
  enum clang_ext_CursorKind result = clang_ext_GetCursorKind(c);
  {
    CAMLlocal1(data);
    data = Val_clang_ext_cursorkind(result);
    CAMLreturn(data);
  }
}

enum clang_ext_TypeKind
Clang_ext_typekind_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return ETK_Invalid;
  case 1: return ETK_Paren;
  case 2: return ETK_Elaborated;
  case 3: return ETK_Unknown;
  }
  failwith_fmt("invalid value for Clang_ext_typekind_val: %d", Int_val(ocaml));
  return ETK_Invalid;
}

value
Val_clang_ext_typekind(enum clang_ext_TypeKind v)
{
  switch (v) {
  case ETK_Invalid: return Val_int(0);
  case ETK_Paren: return Val_int(1);
  case ETK_Elaborated: return Val_int(2);
  case ETK_Unknown: return Val_int(3);
  }
  failwith_fmt("invalid value for Val_clang_ext_typekind: %d", v);
  return Val_int(0);
}

CAMLprim value
clang_ext_GetTypeKind_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXType c;
  c = Cxtype_val(Field(c_ocaml, 0));
  enum clang_ext_TypeKind result = clang_ext_GetTypeKind(c);
  {
    CAMLlocal1(data);
    data = Val_clang_ext_typekind(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_GetInnerType_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXType c;
  c = Cxtype_val(Field(c_ocaml, 0));
  CXType result = clang_ext_GetInnerType(c);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxtype(result));
  Store_field(data, 1, Field(c_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_VariableArrayType_GetSizeExpr_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXType c;
  c = Cxtype_val(Field(c_ocaml, 0));
  CXCursor result = clang_ext_VariableArrayType_GetSizeExpr(c);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(2);
  Store_field(data, 0, Val_cxcursor(result));
  Store_field(data, 1, Field(c_ocaml, 1));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_AsmStmt_GetAsmString_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXCursor c;
  c = Cxcursor_val(Field(c_ocaml, 0));
  CXString result = clang_ext_AsmStmt_GetAsmString(c);
  {
    CAMLlocal1(data);
    data = caml_copy_string(clang_getCString(result));
                    clang_disposeString(result);
    CAMLreturn(data);
  }
}

enum clang_ext_CharacterKind
Clang_ext_characterkind_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return ECK_Ascii;
  case 1: return ECK_Wide;
  case 2: return ECK_UTF8;
  case 3: return ECK_UTF16;
  case 4: return ECK_UTF32;
  }
  failwith_fmt("invalid value for Clang_ext_characterkind_val: %d", Int_val(ocaml));
  return ECK_Ascii;
}

value
Val_clang_ext_characterkind(enum clang_ext_CharacterKind v)
{
  switch (v) {
  case ECK_Ascii: return Val_int(0);
  case ECK_Wide: return Val_int(1);
  case ECK_UTF8: return Val_int(2);
  case ECK_UTF16: return Val_int(3);
  case ECK_UTF32: return Val_int(4);
  }
  failwith_fmt("invalid value for Val_clang_ext_characterkind: %d", v);
  return Val_int(0);
}

CAMLprim value
clang_ext_CharacterLiteral_GetCharacterKind_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXCursor c;
  c = Cxcursor_val(Field(c_ocaml, 0));
  enum clang_ext_CharacterKind result = clang_ext_CharacterLiteral_GetCharacterKind(c);
  {
    CAMLlocal1(data);
    data = Val_clang_ext_characterkind(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_CharacterLiteral_GetValue_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXCursor c;
  c = Cxcursor_val(Field(c_ocaml, 0));
  unsigned int result = clang_ext_CharacterLiteral_GetValue(c);
  {
    CAMLlocal1(data);
    data = Val_int(result);
    CAMLreturn(data);
  }
}

enum clang_ext_UnaryExpr
Clang_ext_unaryexpr_val(value ocaml)
{
  switch (Int_val(ocaml)) {
  case 0: return UETT_SizeOf;
  case 1: return UETT_AlignOf;
  case 2: return UETT_VecStep;
  case 3: return UETT_OpenMPRequiredSimdAlign;
  }
  failwith_fmt("invalid value for Clang_ext_unaryexpr_val: %d", Int_val(ocaml));
  return UETT_SizeOf;
}

value
Val_clang_ext_unaryexpr(enum clang_ext_UnaryExpr v)
{
  switch (v) {
  case UETT_SizeOf: return Val_int(0);
  case UETT_AlignOf: return Val_int(1);
  case UETT_VecStep: return Val_int(2);
  case UETT_OpenMPRequiredSimdAlign: return Val_int(3);
  }
  failwith_fmt("invalid value for Val_clang_ext_unaryexpr: %d", v);
  return Val_int(0);
}

CAMLprim value
clang_ext_UnaryExpr_GetKind_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXCursor c;
  c = Cxcursor_val(Field(c_ocaml, 0));
  enum clang_ext_UnaryExpr result = clang_ext_UnaryExpr_GetKind(c);
  {
    CAMLlocal1(data);
    data = Val_clang_ext_unaryexpr(result);
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_UnaryExpr_GetArgumentType_wrapper(value c_ocaml)
{
  CAMLparam1(c_ocaml);
  CXCursor c;
  c = Cxcursor_val(Field(c_ocaml, 0));
  CXType result = clang_ext_UnaryExpr_GetArgumentType(c);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(1);
  Store_field(data, 0, Val_cxtype(result));
    CAMLreturn(data);
  }
}

CAMLprim value
clang_ext_Type_getNamedType_wrapper(value CT_ocaml)
{
  CAMLparam1(CT_ocaml);
  CXType CT;
  CT = Cxtype_val(Field(CT_ocaml, 0));
  CXType result = clang_ext_Type_getNamedType(CT);
  {
    CAMLlocal1(data);
    data = caml_alloc_tuple(1);
  Store_field(data, 0, Val_cxtype(result));
    CAMLreturn(data);
  }
}

