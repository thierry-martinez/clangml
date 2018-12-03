external get_build_session_timestamp :
  unit -> int = "clang_getBuildSessionTimestamp_wrapper"
type cxvirtualfileoverlay
external virtual_file_overlay_create :
  int -> cxvirtualfileoverlay = "clang_VirtualFileOverlay_create_wrapper"
type cxerrorcode =
  | Failure 
  | Crashed 
  | InvalidArguments 
  | ASTReadError 
external virtual_file_overlay_add_file_mapping :
  cxvirtualfileoverlay ->
    virtual_path:string -> real_path:string -> (unit, cxerrorcode) result =
    "clang_VirtualFileOverlay_addFileMapping_wrapper"
external virtual_file_overlay_set_case_sensitivity :
  cxvirtualfileoverlay -> int -> (unit, cxerrorcode) result =
    "clang_VirtualFileOverlay_setCaseSensitivity_wrapper"
type cxmodulemapdescriptor
external module_map_descriptor_create :
  int -> cxmodulemapdescriptor = "clang_ModuleMapDescriptor_create_wrapper"
external module_map_descriptor_set_framework_module_name :
  cxmodulemapdescriptor -> string -> (unit, cxerrorcode) result =
    "clang_ModuleMapDescriptor_setFrameworkModuleName_wrapper"
external module_map_descriptor_set_umbrella_header :
  cxmodulemapdescriptor -> string -> (unit, cxerrorcode) result =
    "clang_ModuleMapDescriptor_setUmbrellaHeader_wrapper"
external module_map_descriptor_write_to_buffer :
  cxmodulemapdescriptor -> int -> (string, cxerrorcode) result =
    "clang_ModuleMapDescriptor_writeToBuffer_wrapper"
type cxindex
external create_index :
  exclude_declarations_from_pch:bool -> display_diagnostics:bool -> cxindex =
    "clang_createIndex_wrapper"
module Cxglobaloptflags :
sig
  type t
  external (+) : t -> t -> t = "%orint"
  val (-) : t -> t -> t
  external (&) : t -> t -> t = "%andint"
  external ( * ) : t -> t -> t = "%xorint"
  val none : t
  val thread_background_priority_for_indexing : t
  val thread_background_priority_for_editing : t
  val thread_background_priority_for_all : t
end
external cxindex_set_global_options :
  cxindex -> Cxglobaloptflags.t -> unit =
    "clang_CXIndex_setGlobalOptions_wrapper"
external cxindex_get_global_options :
  cxindex -> Cxglobaloptflags.t = "clang_CXIndex_getGlobalOptions_wrapper"
type cxfile
external get_file_name : cxfile -> string = "clang_getFileName_wrapper"
external get_file_time : cxfile -> int = "clang_getFileTime_wrapper"
type cxtranslationunit
external is_file_multiple_include_guarded :
  cxtranslationunit -> cxfile -> bool =
    "clang_isFileMultipleIncludeGuarded_wrapper"
external get_file :
  cxtranslationunit -> string -> cxfile = "clang_getFile_wrapper"
external file_is_equal :
  cxfile -> cxfile -> bool = "clang_File_isEqual_wrapper"
type cxsourcelocation
external get_null_location :
  unit -> cxsourcelocation = "clang_getNullLocation_wrapper"
external equal_locations :
  cxsourcelocation -> cxsourcelocation -> bool =
    "clang_equalLocations_wrapper"
external get_location :
  cxtranslationunit -> cxfile -> line:int -> column:int -> cxsourcelocation =
    "clang_getLocation_wrapper"
external get_location_for_offset :
  cxtranslationunit -> cxfile -> int -> cxsourcelocation =
    "clang_getLocationForOffset_wrapper"
external location_is_in_system_header :
  cxsourcelocation -> bool = "clang_Location_isInSystemHeader_wrapper"
external location_is_from_main_file :
  cxsourcelocation -> bool = "clang_Location_isFromMainFile_wrapper"
type cxsourcerange
external get_null_range :
  unit -> cxsourcerange = "clang_getNullRange_wrapper"
external get_range :
  cxsourcelocation -> cxsourcelocation -> cxsourcerange =
    "clang_getRange_wrapper"
external equal_ranges :
  cxsourcerange -> cxsourcerange -> bool = "clang_equalRanges_wrapper"
external range_is_null : cxsourcerange -> bool = "clang_Range_isNull_wrapper"
external get_expansion_location :
  cxsourcelocation -> (cxfile * int * int * int) =
    "clang_getExpansionLocation_wrapper"
external get_presumed_location :
  cxsourcelocation -> (string * int * int) =
    "clang_getPresumedLocation_wrapper"
external get_instantiation_location :
  cxsourcelocation -> (cxfile * int * int * int) =
    "clang_getInstantiationLocation_wrapper"
external get_spelling_location :
  cxsourcelocation -> (cxfile * int * int * int) =
    "clang_getSpellingLocation_wrapper"
external get_file_location :
  cxsourcelocation -> (cxfile * int * int * int) =
    "clang_getFileLocation_wrapper"
external get_range_start :
  cxsourcerange -> cxsourcelocation = "clang_getRangeStart_wrapper"
external get_range_end :
  cxsourcerange -> cxsourcelocation = "clang_getRangeEnd_wrapper"
external get_skipped_ranges :
  cxtranslationunit -> cxfile -> cxsourcerange array =
    "clang_getSkippedRanges_wrapper"
external get_all_skipped_ranges :
  cxtranslationunit -> cxsourcerange array =
    "clang_getAllSkippedRanges_wrapper"
type cxdiagnosticset
external get_num_diagnostics_in_set :
  cxdiagnosticset -> int = "clang_getNumDiagnosticsInSet_wrapper"
type cxdiagnostic
external get_diagnostic_in_set :
  cxdiagnosticset -> int -> cxdiagnostic = "clang_getDiagnosticInSet_wrapper"
type cxloaddiag_error =
  | Unknown 
  | CannotLoad 
  | InvalidFile 
external load_diagnostics :
  string -> (cxdiagnosticset, (cxloaddiag_error * string)) result =
    "clang_loadDiagnostics_wrapper"
external get_child_diagnostics :
  cxdiagnostic -> cxdiagnosticset = "clang_getChildDiagnostics_wrapper"
external get_num_diagnostics :
  cxtranslationunit -> int = "clang_getNumDiagnostics_wrapper"
external get_diagnostic :
  cxtranslationunit -> int -> cxdiagnostic = "clang_getDiagnostic_wrapper"
external get_diagnostic_set_from_tu :
  cxtranslationunit -> cxdiagnosticset =
    "clang_getDiagnosticSetFromTU_wrapper"
module Cxdiagnosticdisplayoptions :
sig
  type t
  external (+) : t -> t -> t = "%orint"
  val (-) : t -> t -> t
  external (&) : t -> t -> t = "%andint"
  external ( * ) : t -> t -> t = "%xorint"
  val display_source_location : t
  val display_column : t
  val display_source_ranges : t
  val display_option : t
  val display_category_id : t
  val display_category_name : t
end
external format_diagnostic :
  cxdiagnostic -> Cxdiagnosticdisplayoptions.t -> string =
    "clang_formatDiagnostic_wrapper"
external default_diagnostic_display_options :
  unit -> Cxdiagnosticdisplayoptions.t =
    "clang_defaultDiagnosticDisplayOptions_wrapper"
type cxdiagnosticseverity =
  | Ignored 
  | Note 
  | Warning 
  | Error 
  | Fatal 
external get_diagnostic_severity :
  cxdiagnostic -> cxdiagnosticseverity =
    "clang_getDiagnosticSeverity_wrapper"
external get_diagnostic_location :
  cxdiagnostic -> cxsourcelocation = "clang_getDiagnosticLocation_wrapper"
external get_diagnostic_spelling :
  cxdiagnostic -> string = "clang_getDiagnosticSpelling_wrapper"
external get_diagnostic_option :
  cxdiagnostic -> (string * string) = "clang_getDiagnosticOption_wrapper"
external get_diagnostic_category :
  cxdiagnostic -> int = "clang_getDiagnosticCategory_wrapper"
external get_diagnostic_category_text :
  cxdiagnostic -> string = "clang_getDiagnosticCategoryText_wrapper"
external get_diagnostic_num_ranges :
  cxdiagnostic -> int = "clang_getDiagnosticNumRanges_wrapper"
external get_diagnostic_range :
  cxdiagnostic -> int -> cxsourcerange = "clang_getDiagnosticRange_wrapper"
external get_diagnostic_num_fix_its :
  cxdiagnostic -> int = "clang_getDiagnosticNumFixIts_wrapper"
external get_diagnostic_fix_it :
  cxdiagnostic -> int -> cxsourcerange -> (string * cxsourcerange) =
    "clang_getDiagnosticFixIt_wrapper"
external get_translation_unit_spelling :
  cxtranslationunit -> string = "clang_getTranslationUnitSpelling_wrapper"
type cxunsavedfile = {
  filename: string ;
  contents: string }
external create_translation_unit_from_source_file :
  cxindex ->
    string -> string array -> cxunsavedfile array -> cxtranslationunit =
    "clang_createTranslationUnitFromSourceFile_wrapper"
external create_translation_unit :
  cxindex -> string -> cxtranslationunit =
    "clang_createTranslationUnit_wrapper"
external create_translation_unit2 :
  cxindex -> string -> (cxtranslationunit, cxerrorcode) result =
    "clang_createTranslationUnit2_wrapper"
module Cxtranslationunit_flags :
sig
  type t
  external (+) : t -> t -> t = "%orint"
  val (-) : t -> t -> t
  external (&) : t -> t -> t = "%andint"
  external ( * ) : t -> t -> t = "%xorint"
  val none : t
  val detailed_preprocessing_record : t
  val incomplete : t
  val precompiled_preamble : t
  val cache_completion_results : t
  val for_serialization : t
  val cxxchained_pch : t
  val skip_function_bodies : t
  val include_brief_comments_in_code_completion : t
  val create_preamble_on_first_parse : t
  val keep_going : t
end
external default_editing_translation_unit_options :
  unit -> Cxtranslationunit_flags.t =
    "clang_defaultEditingTranslationUnitOptions_wrapper"
external parse_translation_unit :
  cxindex ->
    string ->
      string array ->
        cxunsavedfile array -> Cxtranslationunit_flags.t -> cxtranslationunit
    = "clang_parseTranslationUnit_wrapper"
external parse_translation_unit2 :
  cxindex ->
    string ->
      string array ->
        cxunsavedfile array ->
          Cxtranslationunit_flags.t ->
            (cxtranslationunit, cxerrorcode) result =
    "clang_parseTranslationUnit2_wrapper"
external parse_translation_unit2_full_argv :
  cxindex ->
    string ->
      string array ->
        cxunsavedfile array ->
          Cxtranslationunit_flags.t ->
            (cxtranslationunit, cxerrorcode) result =
    "clang_parseTranslationUnit2FullArgv_wrapper"
external default_save_options :
  cxtranslationunit -> int = "clang_defaultSaveOptions_wrapper"
type cxsaveerror =
  | Unknown 
  | TranslationErrors 
  | InvalidTU 
module Cxsavetranslationunit_flags :
sig
  type t
  external (+) : t -> t -> t = "%orint"
  val (-) : t -> t -> t
  external (&) : t -> t -> t = "%andint"
  external ( * ) : t -> t -> t = "%xorint"
  val none : t
end
external save_translation_unit :
  cxtranslationunit ->
    string -> Cxsavetranslationunit_flags.t -> (unit, cxsaveerror) result =
    "clang_saveTranslationUnit_wrapper"
module Cxreparse_flags :
sig
  type t
  external (+) : t -> t -> t = "%orint"
  val (-) : t -> t -> t
  external (&) : t -> t -> t = "%andint"
  external ( * ) : t -> t -> t = "%xorint"
  val none : t
end
external default_reparse_options :
  cxtranslationunit -> Cxreparse_flags.t =
    "clang_defaultReparseOptions_wrapper"
external reparse_translation_unit :
  cxtranslationunit ->
    cxunsavedfile array -> Cxreparse_flags.t -> (unit, cxerrorcode) result =
    "clang_reparseTranslationUnit_wrapper"
type cxturesourceusagekind =
  | AST 
  | Identifiers 
  | Selectors 
  | GlobalCompletionResults 
  | SourceManagerContentCache 
  | AST_SideTables 
  | SourceManager_Membuffer_Malloc 
  | SourceManager_Membuffer_MMap 
  | ExternalASTSource_Membuffer_Malloc 
  | ExternalASTSource_Membuffer_MMap 
  | Preprocessor 
  | PreprocessingRecord 
  | SourceManager_DataStructures 
  | Preprocessor_HeaderSearch 
external get_turesource_usage_name :
  cxturesourceusagekind -> string = "clang_getTUResourceUsageName_wrapper"
type cxturesourceusage
external get_cxturesource_usage :
  cxtranslationunit -> cxturesourceusage =
    "clang_getCXTUResourceUsage_wrapper"
type cxcursorkind =
  | UnexposedDecl 
  | StructDecl 
  | UnionDecl 
  | ClassDecl 
  | EnumDecl 
  | FieldDecl 
  | EnumConstantDecl 
  | FunctionDecl 
  | VarDecl 
  | ParmDecl 
  | ObjCInterfaceDecl 
  | ObjCCategoryDecl 
  | ObjCProtocolDecl 
  | ObjCPropertyDecl 
  | ObjCIvarDecl 
  | ObjCInstanceMethodDecl 
  | ObjCClassMethodDecl 
  | ObjCImplementationDecl 
  | ObjCCategoryImplDecl 
  | TypedefDecl 
  | CXXMethod 
  | Namespace 
  | LinkageSpec 
  | Constructor 
  | Destructor 
  | ConversionFunction 
  | TemplateTypeParameter 
  | NonTypeTemplateParameter 
  | TemplateTemplateParameter 
  | FunctionTemplate 
  | ClassTemplate 
  | ClassTemplatePartialSpecialization 
  | NamespaceAlias 
  | UsingDirective 
  | UsingDeclaration 
  | TypeAliasDecl 
  | ObjCSynthesizeDecl 
  | ObjCDynamicDecl 
  | CXXAccessSpecifier 
  | FirstRef 
  | ObjCProtocolRef 
  | ObjCClassRef 
  | TypeRef 
  | CXXBaseSpecifier 
  | TemplateRef 
  | NamespaceRef 
  | MemberRef 
  | LabelRef 
  | OverloadedDeclRef 
  | VariableRef 
  | FirstInvalid 
  | NoDeclFound 
  | NotImplemented 
  | InvalidCode 
  | FirstExpr 
  | DeclRefExpr 
  | MemberRefExpr 
  | CallExpr 
  | ObjCMessageExpr 
  | BlockExpr 
  | IntegerLiteral 
  | FloatingLiteral 
  | ImaginaryLiteral 
  | StringLiteral 
  | CharacterLiteral 
  | ParenExpr 
  | UnaryOperator 
  | ArraySubscriptExpr 
  | BinaryOperator 
  | CompoundAssignOperator 
  | ConditionalOperator 
  | CStyleCastExpr 
  | CompoundLiteralExpr 
  | InitListExpr 
  | AddrLabelExpr 
  | StmtExpr 
  | GenericSelectionExpr 
  | GNUNullExpr 
  | CXXStaticCastExpr 
  | CXXDynamicCastExpr 
  | CXXReinterpretCastExpr 
  | CXXConstCastExpr 
  | CXXFunctionalCastExpr 
  | CXXTypeidExpr 
  | CXXBoolLiteralExpr 
  | CXXNullPtrLiteralExpr 
  | CXXThisExpr 
  | CXXThrowExpr 
  | CXXNewExpr 
  | CXXDeleteExpr 
  | UnaryExpr 
  | ObjCStringLiteral 
  | ObjCEncodeExpr 
  | ObjCSelectorExpr 
  | ObjCProtocolExpr 
  | ObjCBridgedCastExpr 
  | PackExpansionExpr 
  | SizeOfPackExpr 
  | LambdaExpr 
  | ObjCBoolLiteralExpr 
  | ObjCSelfExpr 
  | OMPArraySectionExpr 
  | ObjCAvailabilityCheckExpr 
  | FirstStmt 
  | LabelStmt 
  | CompoundStmt 
  | CaseStmt 
  | DefaultStmt 
  | IfStmt 
  | SwitchStmt 
  | WhileStmt 
  | DoStmt 
  | ForStmt 
  | GotoStmt 
  | IndirectGotoStmt 
  | ContinueStmt 
  | BreakStmt 
  | ReturnStmt 
  | GCCAsmStmt 
  | ObjCAtTryStmt 
  | ObjCAtCatchStmt 
  | ObjCAtFinallyStmt 
  | ObjCAtThrowStmt 
  | ObjCAtSynchronizedStmt 
  | ObjCAutoreleasePoolStmt 
  | ObjCForCollectionStmt 
  | CXXCatchStmt 
  | CXXTryStmt 
  | CXXForRangeStmt 
  | SEHTryStmt 
  | SEHExceptStmt 
  | SEHFinallyStmt 
  | MSAsmStmt 
  | NullStmt 
  | DeclStmt 
  | OMPParallelDirective 
  | OMPSimdDirective 
  | OMPForDirective 
  | OMPSectionsDirective 
  | OMPSectionDirective 
  | OMPSingleDirective 
  | OMPParallelForDirective 
  | OMPParallelSectionsDirective 
  | OMPTaskDirective 
  | OMPMasterDirective 
  | OMPCriticalDirective 
  | OMPTaskyieldDirective 
  | OMPBarrierDirective 
  | OMPTaskwaitDirective 
  | OMPFlushDirective 
  | SEHLeaveStmt 
  | OMPOrderedDirective 
  | OMPAtomicDirective 
  | OMPForSimdDirective 
  | OMPParallelForSimdDirective 
  | OMPTargetDirective 
  | OMPTeamsDirective 
  | OMPTaskgroupDirective 
  | OMPCancellationPointDirective 
  | OMPCancelDirective 
  | OMPTargetDataDirective 
  | OMPTaskLoopDirective 
  | OMPTaskLoopSimdDirective 
  | OMPDistributeDirective 
  | OMPTargetEnterDataDirective 
  | OMPTargetExitDataDirective 
  | OMPTargetParallelDirective 
  | OMPTargetParallelForDirective 
  | OMPTargetUpdateDirective 
  | OMPDistributeParallelForDirective 
  | OMPDistributeParallelForSimdDirective 
  | OMPDistributeSimdDirective 
  | OMPTargetParallelForSimdDirective 
  | OMPTargetSimdDirective 
  | OMPTeamsDistributeDirective 
  | OMPTeamsDistributeSimdDirective 
  | OMPTeamsDistributeParallelForSimdDirective 
  | OMPTeamsDistributeParallelForDirective 
  | OMPTargetTeamsDirective 
  | OMPTargetTeamsDistributeDirective 
  | OMPTargetTeamsDistributeParallelForDirective 
  | OMPTargetTeamsDistributeParallelForSimdDirective 
  | OMPTargetTeamsDistributeSimdDirective 
  | TranslationUnit 
  | FirstAttr 
  | IBActionAttr 
  | IBOutletAttr 
  | IBOutletCollectionAttr 
  | CXXFinalAttr 
  | CXXOverrideAttr 
  | AnnotateAttr 
  | AsmLabelAttr 
  | PackedAttr 
  | PureAttr 
  | ConstAttr 
  | NoDuplicateAttr 
  | CUDAConstantAttr 
  | CUDADeviceAttr 
  | CUDAGlobalAttr 
  | CUDAHostAttr 
  | CUDASharedAttr 
  | VisibilityAttr 
  | DLLExport 
  | DLLImport 
  | PreprocessingDirective 
  | MacroDefinition 
  | MacroExpansion 
  | InclusionDirective 
  | ModuleImportDecl 
  | TypeAliasTemplateDecl 
  | StaticAssert 
  | FriendDecl 
  | OverloadCandidate 
type cxcursor
external get_null_cursor : unit -> cxcursor = "clang_getNullCursor_wrapper"
external get_translation_unit_cursor :
  cxtranslationunit -> cxcursor = "clang_getTranslationUnitCursor_wrapper"
external equal_cursors :
  cxcursor -> cxcursor -> bool = "clang_equalCursors_wrapper"
external cursor_is_null : cxcursor -> bool = "clang_Cursor_isNull_wrapper"
external hash_cursor : cxcursor -> int = "clang_hashCursor_wrapper"
external get_cursor_kind :
  cxcursor -> cxcursorkind = "clang_getCursorKind_wrapper"
external is_declaration :
  cxcursorkind -> bool = "clang_isDeclaration_wrapper"
external is_reference : cxcursorkind -> bool = "clang_isReference_wrapper"
external is_expression : cxcursorkind -> bool = "clang_isExpression_wrapper"
external is_statement : cxcursorkind -> bool = "clang_isStatement_wrapper"
external is_attribute : cxcursorkind -> bool = "clang_isAttribute_wrapper"
external cursor_has_attrs : cxcursor -> int = "clang_Cursor_hasAttrs_wrapper"
external is_invalid : cxcursorkind -> bool = "clang_isInvalid_wrapper"
external is_translation_unit :
  cxcursorkind -> bool = "clang_isTranslationUnit_wrapper"
external is_preprocessing :
  cxcursorkind -> bool = "clang_isPreprocessing_wrapper"
external is_unexposed : cxcursorkind -> bool = "clang_isUnexposed_wrapper"
type cxlinkagekind =
  | Invalid 
  | NoLinkage 
  | Internal 
  | UniqueExternal 
  | External 
external get_cursor_linkage :
  cxcursor -> cxlinkagekind = "clang_getCursorLinkage_wrapper"
type cxvisibilitykind =
  | Invalid 
  | Hidden 
  | Protected 
  | Default 
external get_cursor_visibility :
  cxcursor -> cxvisibilitykind = "clang_getCursorVisibility_wrapper"
type cxavailabilitykind =
  | Available 
  | Deprecated 
  | NotAvailable 
  | NotAccessible 
external get_cursor_availability :
  cxcursor -> cxavailabilitykind = "clang_getCursorAvailability_wrapper"
type cxlanguagekind =
  | Invalid 
  | C 
  | ObjC 
  | CPlusPlus 
external get_cursor_language :
  cxcursor -> cxlanguagekind = "clang_getCursorLanguage_wrapper"
external cursor_get_translation_unit :
  cxcursor -> cxtranslationunit = "clang_Cursor_getTranslationUnit_wrapper"
type cxcursorset
external create_cxcursor_set :
  unit -> cxcursorset = "clang_createCXCursorSet_wrapper"
external cxcursor_set_contains :
  cxcursorset -> cxcursor -> int = "clang_CXCursorSet_contains_wrapper"
external cxcursor_set_insert :
  cxcursorset -> cxcursor -> int = "clang_CXCursorSet_insert_wrapper"
external get_cursor_semantic_parent :
  cxcursor -> cxcursor = "clang_getCursorSemanticParent_wrapper"
external get_cursor_lexical_parent :
  cxcursor -> cxcursor = "clang_getCursorLexicalParent_wrapper"
external get_overridden_cursors :
  cxcursor -> string = "clang_getOverriddenCursors_wrapper"
external get_included_file :
  cxcursor -> cxfile = "clang_getIncludedFile_wrapper"
external get_cursor :
  cxtranslationunit -> cxsourcelocation -> cxcursor =
    "clang_getCursor_wrapper"
external get_cursor_location :
  cxcursor -> cxsourcelocation = "clang_getCursorLocation_wrapper"
external get_cursor_extent :
  cxcursor -> cxsourcerange = "clang_getCursorExtent_wrapper"
type cxtypekind =
  | Invalid 
  | Unexposed 
  | Void 
  | Bool 
  | Char_U 
  | UChar 
  | Char16 
  | Char32 
  | UShort 
  | UInt 
  | ULong 
  | ULongLong 
  | UInt128 
  | Char_S 
  | SChar 
  | WChar 
  | Short 
  | Int 
  | Long 
  | LongLong 
  | Int128 
  | Float 
  | Double 
  | LongDouble 
  | NullPtr 
  | Overload 
  | Dependent 
  | ObjCId 
  | ObjCClass 
  | ObjCSel 
  | Float128 
  | Complex 
  | Pointer 
  | BlockPointer 
  | LValueReference 
  | RValueReference 
  | Record 
  | Enum 
  | Typedef 
  | ObjCInterface 
  | ObjCObjectPointer 
  | FunctionNoProto 
  | FunctionProto 
  | ConstantArray 
  | Vector 
  | IncompleteArray 
  | VariableArray 
  | DependentSizedArray 
  | MemberPointer 
  | Auto 
  | Elaborated 
type cxtype
external get_type_kind : cxtype -> cxtypekind = "clang_getTypeKind_wrapper"
external get_cursor_type : cxcursor -> cxtype = "clang_getCursorType_wrapper"
external get_type_spelling :
  cxtype -> string = "clang_getTypeSpelling_wrapper"
external get_typedef_decl_underlying_type :
  cxcursor -> cxtype = "clang_getTypedefDeclUnderlyingType_wrapper"
external get_enum_decl_integer_type :
  cxcursor -> cxtype = "clang_getEnumDeclIntegerType_wrapper"
external get_enum_constant_decl_value :
  cxcursor -> int = "clang_getEnumConstantDeclValue_wrapper"
external get_enum_constant_decl_unsigned_value :
  cxcursor -> int = "clang_getEnumConstantDeclUnsignedValue_wrapper"
external get_field_decl_bit_width :
  cxcursor -> int = "clang_getFieldDeclBitWidth_wrapper"
external cursor_get_num_arguments :
  cxcursor -> int = "clang_Cursor_getNumArguments_wrapper"
external cursor_get_argument :
  cxcursor -> int -> cxcursor = "clang_Cursor_getArgument_wrapper"
external cursor_get_num_template_arguments :
  cxcursor -> int = "clang_Cursor_getNumTemplateArguments_wrapper"
type cxtemplateargumentkind =
  | Null 
  | Type 
  | Declaration 
  | NullPtr 
  | Integral 
  | Template 
  | TemplateExpansion 
  | Expression 
  | Pack 
  | Invalid 
external cursor_get_template_argument_kind :
  cxcursor -> int -> cxtemplateargumentkind =
    "clang_Cursor_getTemplateArgumentKind_wrapper"
external cursor_get_template_argument_type :
  cxcursor -> int -> cxtype = "clang_Cursor_getTemplateArgumentType_wrapper"
external cursor_get_template_argument_value :
  cxcursor -> int -> int = "clang_Cursor_getTemplateArgumentValue_wrapper"
external cursor_get_template_argument_unsigned_value :
  cxcursor -> int -> int =
    "clang_Cursor_getTemplateArgumentUnsignedValue_wrapper"
external equal_types : cxtype -> cxtype -> bool = "clang_equalTypes_wrapper"
external get_canonical_type :
  cxtype -> cxtype = "clang_getCanonicalType_wrapper"
external is_const_qualified_type :
  cxtype -> bool = "clang_isConstQualifiedType_wrapper"
external cursor_is_macro_function_like :
  cxcursor -> bool = "clang_Cursor_isMacroFunctionLike_wrapper"
external cursor_is_macro_builtin :
  cxcursor -> bool = "clang_Cursor_isMacroBuiltin_wrapper"
external cursor_is_function_inlined :
  cxcursor -> bool = "clang_Cursor_isFunctionInlined_wrapper"
external is_volatile_qualified_type :
  cxtype -> bool = "clang_isVolatileQualifiedType_wrapper"
external is_restrict_qualified_type :
  cxtype -> bool = "clang_isRestrictQualifiedType_wrapper"
external get_pointee_type : cxtype -> cxtype = "clang_getPointeeType_wrapper"
external get_type_declaration :
  cxtype -> cxcursor = "clang_getTypeDeclaration_wrapper"
external get_decl_obj_ctype_encoding :
  cxcursor -> string = "clang_getDeclObjCTypeEncoding_wrapper"
external type_get_obj_cencoding :
  cxtype -> string = "clang_Type_getObjCEncoding_wrapper"
external get_type_kind_spelling :
  cxtypekind -> string = "clang_getTypeKindSpelling_wrapper"
type cxcallingconv =
  | Default 
  | C 
  | X86StdCall 
  | X86FastCall 
  | X86ThisCall 
  | X86Pascal 
  | AAPCS 
  | AAPCS_VFP 
  | X86RegCall 
  | IntelOclBicc 
  | X86_64Win64 
  | X86_64SysV 
  | X86VectorCall 
  | Swift 
  | PreserveMost 
  | PreserveAll 
  | Invalid 
  | Unexposed 
external get_function_type_calling_conv :
  cxtype -> cxcallingconv = "clang_getFunctionTypeCallingConv_wrapper"
external get_result_type : cxtype -> cxtype = "clang_getResultType_wrapper"
external get_num_arg_types : cxtype -> int = "clang_getNumArgTypes_wrapper"
external get_arg_type : cxtype -> int -> cxtype = "clang_getArgType_wrapper"
external is_function_type_variadic :
  cxtype -> bool = "clang_isFunctionTypeVariadic_wrapper"
external get_cursor_result_type :
  cxcursor -> cxtype = "clang_getCursorResultType_wrapper"
external is_podtype : cxtype -> bool = "clang_isPODType_wrapper"
external get_element_type : cxtype -> cxtype = "clang_getElementType_wrapper"
external get_num_elements : cxtype -> int = "clang_getNumElements_wrapper"
external get_array_element_type :
  cxtype -> cxtype = "clang_getArrayElementType_wrapper"
external get_array_size : cxtype -> int = "clang_getArraySize_wrapper"
external type_get_named_type :
  cxtype -> cxtype = "clang_Type_getNamedType_wrapper"
external type_get_align_of : cxtype -> int = "clang_Type_getAlignOf_wrapper"
external type_get_class_type :
  cxtype -> cxtype = "clang_Type_getClassType_wrapper"
external type_get_size_of : cxtype -> int = "clang_Type_getSizeOf_wrapper"
external type_get_offset_of :
  cxtype -> string -> int = "clang_Type_getOffsetOf_wrapper"
external cursor_get_offset_of_field :
  cxcursor -> int = "clang_Cursor_getOffsetOfField_wrapper"
external cursor_is_anonymous :
  cxcursor -> bool = "clang_Cursor_isAnonymous_wrapper"
external type_get_num_template_arguments :
  cxtype -> int = "clang_Type_getNumTemplateArguments_wrapper"
external type_get_template_argument_as_type :
  cxtype -> int -> cxtype = "clang_Type_getTemplateArgumentAsType_wrapper"
type cxrefqualifierkind =
  | None 
  | LValue 
  | RValue 
external type_get_cxxref_qualifier :
  cxtype -> cxrefqualifierkind = "clang_Type_getCXXRefQualifier_wrapper"
external cursor_is_bit_field :
  cxcursor -> bool = "clang_Cursor_isBitField_wrapper"
external is_virtual_base : cxcursor -> bool = "clang_isVirtualBase_wrapper"
type cx_cxxaccessspecifier =
  | CXXInvalidAccessSpecifier 
  | CXXPublic 
  | CXXProtected 
  | CXXPrivate 
external get_cxxaccess_specifier :
  cxcursor -> cx_cxxaccessspecifier = "clang_getCXXAccessSpecifier_wrapper"
type cx_storageclass =
  | Invalid 
  | None 
  | Extern 
  | Static 
  | PrivateExtern 
  | OpenCLWorkGroupLocal 
  | Auto 
  | Register 
external cursor_get_storage_class :
  cxcursor -> cx_storageclass = "clang_Cursor_getStorageClass_wrapper"
external get_num_overloaded_decls :
  cxcursor -> int = "clang_getNumOverloadedDecls_wrapper"
external get_overloaded_decl :
  cxcursor -> int -> cxcursor = "clang_getOverloadedDecl_wrapper"
external get_iboutlet_collection_type :
  cxcursor -> cxtype = "clang_getIBOutletCollectionType_wrapper"
type cxchildvisitresult =
  | Break 
  | Continue 
  | Recurse 
external visit_children :
  cxcursor -> (cxcursor -> cxcursor -> cxchildvisitresult) -> bool =
    "clang_visitChildren_wrapper"
external get_cursor_usr : cxcursor -> string = "clang_getCursorUSR_wrapper"
external get_cursor_spelling :
  cxcursor -> string = "clang_getCursorSpelling_wrapper"
external cursor_get_spelling_name_range :
  cxcursor -> piece_index:int -> options:int -> cxsourcerange =
    "clang_Cursor_getSpellingNameRange_wrapper"
external get_cursor_display_name :
  cxcursor -> string = "clang_getCursorDisplayName_wrapper"
external get_cursor_referenced :
  cxcursor -> cxcursor = "clang_getCursorReferenced_wrapper"
external get_cursor_definition :
  cxcursor -> cxcursor = "clang_getCursorDefinition_wrapper"
external is_cursor_definition :
  cxcursor -> bool = "clang_isCursorDefinition_wrapper"
external get_canonical_cursor :
  cxcursor -> cxcursor = "clang_getCanonicalCursor_wrapper"
external cursor_get_obj_cselector_index :
  cxcursor -> int = "clang_Cursor_getObjCSelectorIndex_wrapper"
external cursor_is_dynamic_call :
  cxcursor -> bool = "clang_Cursor_isDynamicCall_wrapper"
external cursor_get_receiver_type :
  cxcursor -> cxtype = "clang_Cursor_getReceiverType_wrapper"
external cursor_get_obj_cproperty_attributes :
  cxcursor -> int -> int = "clang_Cursor_getObjCPropertyAttributes_wrapper"
external cursor_get_obj_cdecl_qualifiers :
  cxcursor -> int = "clang_Cursor_getObjCDeclQualifiers_wrapper"
external cursor_is_obj_coptional :
  cxcursor -> bool = "clang_Cursor_isObjCOptional_wrapper"
external cursor_is_variadic :
  cxcursor -> bool = "clang_Cursor_isVariadic_wrapper"
external cursor_get_comment_range :
  cxcursor -> cxsourcerange = "clang_Cursor_getCommentRange_wrapper"
external cursor_get_raw_comment_text :
  cxcursor -> string = "clang_Cursor_getRawCommentText_wrapper"
external cursor_get_brief_comment_text :
  cxcursor -> string = "clang_Cursor_getBriefCommentText_wrapper"
external cursor_get_mangling :
  cxcursor -> string = "clang_Cursor_getMangling_wrapper"
external cursor_get_cxxmanglings :
  cxcursor -> string array = "clang_Cursor_getCXXManglings_wrapper"
type cxmodule
external cursor_get_module :
  cxcursor -> cxmodule = "clang_Cursor_getModule_wrapper"
external get_module_for_file :
  cxtranslationunit -> cxfile -> cxmodule = "clang_getModuleForFile_wrapper"
external module_get_astfile :
  cxmodule -> cxfile = "clang_Module_getASTFile_wrapper"
external module_get_parent :
  cxmodule -> cxmodule = "clang_Module_getParent_wrapper"
external module_get_name :
  cxmodule -> string = "clang_Module_getName_wrapper"
external module_get_full_name :
  cxmodule -> string = "clang_Module_getFullName_wrapper"
external module_is_system :
  cxmodule -> bool = "clang_Module_isSystem_wrapper"
external module_get_num_top_level_headers :
  cxtranslationunit -> cxmodule -> int =
    "clang_Module_getNumTopLevelHeaders_wrapper"
external module_get_top_level_header :
  cxtranslationunit -> cxmodule -> int -> cxfile =
    "clang_Module_getTopLevelHeader_wrapper"
external cxxconstructor_is_converting_constructor :
  cxcursor -> bool = "clang_CXXConstructor_isConvertingConstructor_wrapper"
external cxxconstructor_is_copy_constructor :
  cxcursor -> bool = "clang_CXXConstructor_isCopyConstructor_wrapper"
external cxxconstructor_is_default_constructor :
  cxcursor -> bool = "clang_CXXConstructor_isDefaultConstructor_wrapper"
external cxxconstructor_is_move_constructor :
  cxcursor -> bool = "clang_CXXConstructor_isMoveConstructor_wrapper"
external cxxfield_is_mutable :
  cxcursor -> bool = "clang_CXXField_isMutable_wrapper"
external cxxmethod_is_defaulted :
  cxcursor -> bool = "clang_CXXMethod_isDefaulted_wrapper"
external cxxmethod_is_pure_virtual :
  cxcursor -> bool = "clang_CXXMethod_isPureVirtual_wrapper"
external cxxmethod_is_static :
  cxcursor -> bool = "clang_CXXMethod_isStatic_wrapper"
external cxxmethod_is_virtual :
  cxcursor -> bool = "clang_CXXMethod_isVirtual_wrapper"
external cxxmethod_is_const :
  cxcursor -> bool = "clang_CXXMethod_isConst_wrapper"
external get_template_cursor_kind :
  cxcursor -> cxcursorkind = "clang_getTemplateCursorKind_wrapper"
external get_specialized_cursor_template :
  cxcursor -> cxcursor = "clang_getSpecializedCursorTemplate_wrapper"
external get_cursor_reference_name_range :
  cxcursor -> name_flags:int -> piece_index:int -> cxsourcerange =
    "clang_getCursorReferenceNameRange_wrapper"
external get_cursor_kind_spelling :
  cxcursorkind -> string = "clang_getCursorKindSpelling_wrapper"
external enable_stack_traces :
  unit -> unit = "clang_enableStackTraces_wrapper"
type cxcompletionchunkkind =
  | Optional 
  | TypedText 
  | Text 
  | Placeholder 
  | Informative 
  | CurrentParameter 
  | LeftParen 
  | RightParen 
  | LeftBracket 
  | RightBracket 
  | LeftBrace 
  | RightBrace 
  | LeftAngle 
  | RightAngle 
  | Comma 
  | ResultType 
  | Colon 
  | SemiColon 
  | Equal 
  | HorizontalSpace 
  | VerticalSpace 
type cxcompletionstring
external get_completion_chunk_kind :
  cxcompletionstring -> int -> cxcompletionchunkkind =
    "clang_getCompletionChunkKind_wrapper"
external get_completion_chunk_text :
  cxcompletionstring -> int -> string =
    "clang_getCompletionChunkText_wrapper"
external get_completion_chunk_completion_string :
  cxcompletionstring -> int -> cxcompletionstring =
    "clang_getCompletionChunkCompletionString_wrapper"
external get_num_completion_chunks :
  cxcompletionstring -> int = "clang_getNumCompletionChunks_wrapper"
external get_completion_priority :
  cxcompletionstring -> int = "clang_getCompletionPriority_wrapper"
external get_completion_availability :
  cxcompletionstring -> cxavailabilitykind =
    "clang_getCompletionAvailability_wrapper"
external get_completion_num_annotations :
  cxcompletionstring -> int = "clang_getCompletionNumAnnotations_wrapper"
external get_completion_annotation :
  cxcompletionstring -> int -> string =
    "clang_getCompletionAnnotation_wrapper"
external get_completion_parent :
  cxcompletionstring -> string = "clang_getCompletionParent_wrapper"
external get_completion_brief_comment :
  cxcompletionstring -> string = "clang_getCompletionBriefComment_wrapper"
external get_cursor_completion_string :
  cxcursor -> cxcompletionstring = "clang_getCursorCompletionString_wrapper"
external default_code_complete_options :
  unit -> int = "clang_defaultCodeCompleteOptions_wrapper"
external get_clang_version : unit -> string = "clang_getClangVersion_wrapper"
external toggle_crash_recovery :
  int -> unit = "clang_toggleCrashRecovery_wrapper"
type cxevalresult
external cursor_evaluate :
  cxcursor -> cxevalresult = "clang_Cursor_Evaluate_wrapper"
type cxevalresultkind =
  | Int 
  | Float 
  | ObjCStrLiteral 
  | StrLiteral 
  | CFStr 
  | Other 
  | UnExposed 
external eval_result_get_kind :
  cxevalresult -> cxevalresultkind = "clang_EvalResult_getKind_wrapper"
external eval_result_get_as_int :
  cxevalresult -> int = "clang_EvalResult_getAsInt_wrapper"
external eval_result_get_as_long_long :
  cxevalresult -> int = "clang_EvalResult_getAsLongLong_wrapper"
external eval_result_is_unsigned_int :
  cxevalresult -> bool = "clang_EvalResult_isUnsignedInt_wrapper"
external eval_result_get_as_unsigned :
  cxevalresult -> int = "clang_EvalResult_getAsUnsigned_wrapper"
external eval_result_get_as_double :
  cxevalresult -> float = "clang_EvalResult_getAsDouble_wrapper"
external eval_result_get_as_str :
  cxevalresult -> string = "clang_EvalResult_getAsStr_wrapper"
type cxremapping
external get_remappings :
  string -> cxremapping = "clang_getRemappings_wrapper"
external get_remappings_from_file_list :
  string array -> cxremapping = "clang_getRemappingsFromFileList_wrapper"
external remap_get_num_files :
  cxremapping -> int = "clang_remap_getNumFiles_wrapper"
type cxindexaction
external index_action_create :
  cxindex -> cxindexaction = "clang_IndexAction_create_wrapper"
type cxvisitorresult =
  | Break 
  | Continue 
external type_visit_fields :
  cxtype -> (cxcursor -> cxvisitorresult) -> bool =
    "clang_Type_visitFields_wrapper"
type cxint
external ext_integer_literal_get_value :
  cxcursor -> cxint = "clang_ext_IntegerLiteral_getValue_wrapper"
external ext_int_is_valid : cxint -> bool = "clang_ext_Int_isValid_wrapper"
external ext_int_to_string :
  cxint -> int -> bool -> string = "clang_ext_Int_toString_wrapper"
external ext_int_round_to_double :
  cxint -> bool -> float = "clang_ext_Int_roundToDouble_wrapper"
external ext_int_bits_to_float :
  cxint -> float = "clang_ext_Int_bitsToFloat_wrapper"
external ext_int_get_bit_width :
  cxint -> int = "clang_ext_Int_getBitWidth_wrapper"
external ext_int_get_active_bits :
  cxint -> int = "clang_ext_Int_getActiveBits_wrapper"
external ext_int_get_min_signed_bits :
  cxint -> int = "clang_ext_Int_getMinSignedBits_wrapper"
external ext_int_get_bool_value :
  cxint -> bool = "clang_ext_Int_getBoolValue_wrapper"
external ext_int_get_sext_value :
  cxint -> Int64.t = "clang_ext_Int_getSExtValue_wrapper"
type cxfloat
external ext_floating_literal_get_value :
  cxcursor -> cxfloat = "clang_ext_FloatingLiteral_getValue_wrapper"
external ext_float_is_valid :
  cxfloat -> bool = "clang_ext_Float_isValid_wrapper"
external ext_float_to_string :
  cxfloat -> string = "clang_ext_Float_toString_wrapper"
external ext_float_convert_to_double :
  cxfloat -> float = "clang_ext_Float_convertToDouble_wrapper"
external ext_string_literal_get_string :
  cxcursor -> string = "clang_ext_StringLiteral_GetString_wrapper"
type clang_ext_unaryoperatorkind =
  | PostInc 
  | PostDec 
  | PreInc 
  | PreDec 
  | AddrOf 
  | Deref 
  | Plus 
  | Minus 
  | Not 
  | LNot 
  | Real 
  | Imag 
  | Extension 
  | Coawait 
external ext_unary_operator_get_opcode :
  cxcursor -> clang_ext_unaryoperatorkind =
    "clang_ext_UnaryOperator_getOpcode_wrapper"
external ext_unary_operator_get_opcode_spelling :
  clang_ext_unaryoperatorkind -> string =
    "clang_ext_UnaryOperator_getOpcodeSpelling_wrapper"
type clang_ext_binaryoperatorkind =
  | PtrMemD 
  | PtrMemI 
  | Mul 
  | Div 
  | Rem 
  | Add 
  | Sub 
  | Shl 
  | Shr 
  | LT 
  | GT 
  | LE 
  | GE 
  | EQ 
  | NE 
  | And 
  | Xor 
  | Or 
  | LAnd 
  | LOr 
  | Assign 
  | MulAssign 
  | DivAssign 
  | RemAssign 
  | AddAssign 
  | SubAssign 
  | ShlAssign 
  | ShrAssign 
  | AndAssign 
  | XorAssign 
  | OrAssign 
  | Comma 
external ext_binary_operator_get_opcode :
  cxcursor -> clang_ext_binaryoperatorkind =
    "clang_ext_BinaryOperator_getOpcode_wrapper"
external ext_binary_operator_get_opcode_spelling :
  clang_ext_binaryoperatorkind -> string =
    "clang_ext_BinaryOperator_getOpcodeSpelling_wrapper"
external ext_for_stmt_get_children_set :
  cxcursor -> int = "clang_ext_ForStmt_getChildrenSet_wrapper"
external ext_if_stmt_get_children_set :
  cxcursor -> int = "clang_ext_IfStmt_getChildrenSet_wrapper"
external ext_if_stmt_get_init :
  cxcursor -> cxcursor = "clang_ext_IfStmt_getInit_wrapper"
external ext_switch_stmt_get_children_set :
  cxcursor -> int = "clang_ext_SwitchStmt_getChildrenSet_wrapper"
external ext_switch_stmt_get_init :
  cxcursor -> cxcursor = "clang_ext_SwitchStmt_getInit_wrapper"
external ext_while_stmt_get_children_set :
  cxcursor -> int = "clang_ext_WhileStmt_getChildrenSet_wrapper"
type clang_ext_elaboratedtypekeyword =
  | Struct 
  | Interface 
  | Union 
  | Class 
  | Enum 
  | Typename 
  | None 
external ext_elaborated_type_get_keyword :
  cxtype -> clang_ext_elaboratedtypekeyword =
    "clang_ext_ElaboratedType_getKeyword_wrapper"
external ext_elaborated_type_get_keyword_spelling :
  clang_ext_elaboratedtypekeyword -> string =
    "clang_ext_ElaboratedType_getKeywordSpelling_wrapper"
external ext_var_decl_has_init :
  cxcursor -> bool = "clang_ext_VarDecl_hasInit_wrapper"
external ext_member_ref_expr_is_arrow :
  cxcursor -> bool = "clang_ext_MemberRefExpr_isArrow_wrapper"
external ext_stmt_get_class_name :
  cxcursor -> string = "clang_ext_Stmt_GetClassName_wrapper"
external ext_stmt_get_class_kind :
  cxcursor -> int = "clang_ext_Stmt_GetClassKind_wrapper"
type clang_ext_cursorkind =
  | ImplicitCastExpr 
  | BinaryConditionalOperator 
  | Unknown 
external ext_get_cursor_kind :
  cxcursor -> clang_ext_cursorkind = "clang_ext_GetCursorKind_wrapper"
type clang_ext_typekind =
  | Invalid 
  | Paren 
  | Unknown 
external ext_get_type_kind :
  cxtype -> clang_ext_typekind = "clang_ext_GetTypeKind_wrapper"
external ext_get_inner_type :
  cxtype -> cxtype = "clang_ext_GetInnerType_wrapper"
external ext_variable_array_type_get_size_expr :
  cxtype -> cxcursor = "clang_ext_VariableArrayType_GetSizeExpr_wrapper"
external ext_asm_stmt_get_asm_string :
  cxcursor -> string = "clang_ext_AsmStmt_GetAsmString_wrapper"
