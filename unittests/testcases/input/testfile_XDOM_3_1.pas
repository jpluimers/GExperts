unit XDOM_3_1;

// get rid of these annoying warnings because we are not going to fix them
{$WARNINGS off}
{$HINTS off}

// XDOM 3.1.14
// Extended Document Object Model 3.1.14
// Delphi 5/6/7 and Kylix Implementation
// September 2004
//
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License Version
// 1.1 (the "License"); you may not use this file except in compliance with
// the License. You may obtain a copy of the License at
// "http://www.mozilla.org/MPL/"
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
// the specific language governing rights and limitations under the License.
//
// The Original Code is "XDOM_3_1.pas".
//
// The Initial Developer of the Original Code is Dieter Köhler (Heidelberg,
// Germany, "http://www.philo.de/"). Portions created by the Initial Developer
// are Copyright (C) 1999-2004 Dieter Köhler. All Rights Reserved.
//
// Alternatively, the contents of this file may be used under the terms of the
// GNU General Public License Version 2 or later (the "GPL"), in which case the
// provisions of the GPL are applicable instead of those above. If you wish to
// allow use of your version of this file only under the terms of the GPL, and
// not to allow others to use your version of this file under the terms of the
// MPL, indicate your decision by deleting the provisions above and replace them
// with the notice and other provisions required by the GPL. If you do not delete
// the provisions above, a recipient may use your version of this file under the
// terms of any one of the MPL or the GPL.

{$DEFINE INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
{$DEFINE IGNORE_DOCUMENT_FORMAT}

{$WARNINGS OFF}

{$IFDEF WIN32}
  {$IFNDEF VER140}
    {$DEFINE MSWINDOWS}
  {$ENDIF}
{$ENDIF}
{$IFDEF WIN16}
  {$DEFINE MSWINDOWS}
{$ENDIF}
{$IFDEF VER140}
  {$DEFINE VER140+}
{$ENDIF}
{$IFDEF VER150}
  {$DEFINE VER140+}
{$ENDIF}

interface

uses dialogs,
  cUnicodeCodecs, ParserUtils, TreeUtils, WideStrUtils, MiscUtils, 
    // The above units are contained in the Open XML Utilities package 1.x
    // available at "http://www.philo.de/xml/".
  {$IFDEF MSWINDOWS}
    {$IFDEF VER140+} Types,
    {$ELSE} Windows, {$ENDIF}
  {$ENDIF}
  {$IFDEF LINUX}
    Types,
  {$ENDIF}
  SysUtils, Classes;

type
  EDomException = class(Exception);

  EIndex_Size_Err = class(EdomException);
  EDomstring_Size_Err = class(EdomException);
  EHierarchy_Request_Err = class(EdomException);
  EWrong_Document_Err = class(EdomException);
  EInvalid_Character_Err = class(EdomException);
  ENo_Data_Allowed_Err = class(EdomException);
  ENo_Modification_Allowed_Err = class(EdomException);
  ENot_Found_Err = class(EdomException);
  ENot_Supported_Err = class(EdomException);
  EInuse_Err = class(EdomException);
  EInvalid_State_Err = class(EdomException);
  ESyntax_Err = class(EdomException);
  EInvalid_Modification_Err = class(EdomException);
  ENamespace_Err = class(EdomException);
  EInvalid_Access_Err = class(EdomException);
{$ifndef IGNORE_DOCUMENT_FORMAT}
  EUnknown_Document_Format_Err = class(EdomException);
{$endif}
  EWrong_DOM_Implementation_Err = class(EdomException);

  EASException = class(EdomException);
  EAS_Wrong_Element_Decl_Err = class(EASException);

  EParserException = class(EdomException);

  EXPath_Exception = class(EdomException);
  EXPath_Invalid_Expression_Err = class(EXPath_Exception);
  EXPath_Invalid_Function_Call_Err = class(EXPath_Exception);
  EXPath_Type_Err = class(EXPath_Exception);


  TXmlErrorType = (

    // Remark: The order and number of this error types is likely subject to
    //         change in future XDOM versions.  Therefore, you are strongly
    //         adviced to refer to error types by using the constants below,
    //         but avoid using their numerical equivalents!

    ET_NONE, // No Error

    ET_DOUBLE_ATTLISTDECL,
    ET_DOUBLE_ATTDEF,
    ET_DOUBLE_ENTITY_DECL,
    ET_DOUBLE_PARAMETER_ENTITY_DECL,
    ET_UNUSABLE_ENTITY_DECL,

    ET_ATTRIBUTE_DEFAULT_TYPE_MISMATCH,
    ET_ATTRIBUTE_DEFINITION_NOT_FOUND,
    ET_ATTRIBUTE_TYPE_MISMATCH,
    ET_DOCTYPE_NOT_FOUND,
    ET_DUPLICATE_ELEMENT_TYPE_DECL,
    ET_DUPLICATE_ENUMERATION_TOKEN,
    ET_DUPLICATE_ID_ON_ELEMENT_TYPE,
    ET_DUPLICATE_ID_VALUE,
    ET_DUPLICATE_NAME_IN_MIXED_CONTENT,
    ET_DUPLICATE_NOTATION_DECL,
    ET_DUPLICATE_NOTATION_ON_ELEMENT_TYPE,
    ET_DUPLICATE_NOTATION_TOKEN,
    ET_DUPLICATE_TOKENS,
    ET_FIXED_ATTRIBUTE_MISMATCH,
    ET_ID_NEITHER_IMPLIED_NOR_REQUIRED,
    ET_ELEMENT_DECLARED_EMPTY_HAS_CONTENT,
    ET_ELEMENT_TYPE_DECL_NOT_FOUND,
    ET_ELEMENT_WITH_ILLEGAL_ELEMENT_CONTENT,
    ET_ELEMENT_WITH_ILLEGAL_MIXED_CONTENT,
    ET_ENTITY_DECL_NOT_FOUND,
    ET_EXTERNAL_SUBSET_NOT_FOUND,
    ET_NONDETERMINISTIC_ELEMENT_CONTENT_MODEL,
    ET_NOTATION_ON_EMPTY_ELEMENT,
    ET_PARAMETER_ENTITY_DECL_NOT_FOUND,
    ET_REQUIRED_ATTRIBUTE_NOT_FOUND,
    ET_TARGET_ID_VALUE_NOT_FOUND,
    ET_TARGET_UNPARSED_ENTITY_NOT_FOUND,
    ET_UNDECLARED_NOTATION_NAME,
    ET_UNRESOLVABLE_ENTITY_REFERENCE,
    ET_UNRESOLVABLE_PARAMETER_ENTITY_REFERENCE,
    ET_WRONG_DECL_OF_PREDEFINED_ENTITY,
    ET_WRONG_ROOT_ELEMENT_TYPE,

    ET_ATTRIBUTE_VALUE_REFERS_TO_EXTERNAL_ENTITY,
    ET_CDATA_START_EXPECTED,
    ET_COMMENT_START_EXPECTED,
    ET_DOCTYPE_START_EXPECTED,
    ET_DOUBLE_ATTRIBUTE_NAME,
    ET_DOUBLE_EQUALITY_SIGN,
    ET_DOUBLE_HYPHEN_IN_COMMENT,
    ET_DOUBLE_ROOT_ELEMENT,
    ET_HYPHEN_AT_COMMENT_END,
    ET_INVALID_ATTRIBUTE_NAME,
    ET_INVALID_ATTRIBUTE_VALUE,
    ET_INVALID_CDATA_SECTION,
    ET_INVALID_CHARACTER,
    ET_INVALID_CHARREF,
    ET_INVALID_COMMENT,
    ET_INVALID_ELEMENT_NAME,
    ET_INVALID_ENTITY_NAME,
    ET_INVALID_PROCESSING_INSTRUCTION,
    ET_INVALID_PUBID_LITERAL,
    ET_INVALID_SYSTEM_LITERAL,
    ET_INVALID_TEXT_DECL,
    ET_INVALID_XML_DECL,
    ET_LEFT_SQUARE_BRACKET_EXPECTED,
    ET_LT_IN_ATTRIBUTE_VALUE,
    ET_MISSING_ELEMENT_NAME,
    ET_MISSING_ENTITY_NAME,
    ET_MISSING_END_TAG,
    ET_MISSING_EQUALITY_SIGN,
    ET_MISSING_QUOTATION_MARK,
    ET_MISSING_START_TAG,
    ET_MISSING_WHITE_SPACE,
    ET_NO_PROPER_MARKUP_REFERENCED,
    ET_NOT_IN_ROOT,
    ET_PUBLIC_KEYWORD_EXPECTED,
    ET_QUOTATION_MARK_EXPECTED,
    ET_RECURSIVE_REFERENCE,
    ET_REFERS_TO_UNPARSED_ENTITY,
    ET_RIGHT_SQUARE_BRACKET_EXPECTED,
    ET_ROOT_NOT_FOUND,
    ET_SYSTEM_KEYWORD_EXPECTED,
    ET_UNCLOSED_CDATA_SECTION,
    ET_UNCLOSED_CHARREF,
    ET_UNCLOSED_COMMENT,
    ET_UNCLOSED_DOCTYPE,
    ET_UNCLOSED_ELEMENT,
    ET_UNCLOSED_ENTITY_REF,
    ET_UNCLOSED_PROCESSING_INSTRUCTION,
    ET_WRONG_ORDER,

    ET_ATTLIST_DECL_START_EXPECTED,
    ET_INVALID_ATTLIST_DECL_NAME,
    ET_DOUBLE_DOCTYPE,
    ET_CONDITIONAL_SECTION_NOT_ALLOWED,
    ET_ELEMENT_DECL_START_EXPECTED,
    ET_ENTITY_DECL_START_EXPECTED,
    ET_INVALID_ATTRIBUTE_DECL,
    ET_INVALID_CONDITIONAL_SECTION,
    ET_INVALID_DOCTYPE,
    ET_INVALID_ELEMENT_DECL,
    ET_INVALID_ENTITY_DECL,
    ET_INVALID_MARKUP_DECL,
    ET_INVALID_NOTATION_DECL,
    ET_INVALID_PARAMETER_ENTITY_DECL,
    ET_NOTATION_DECL_START_EXPECTED,
    ET_UNCLOSED_ATTLIST_DECL,
    ET_UNCLOSED_CONDITIONAL_SECTION,
    ET_UNCLOSED_ELEMENT_DECL,
    ET_UNCLOSED_ENTITY_DECL,
    ET_UNCLOSED_NOTATION_DECL,
    ET_UNCLOSED_PARAMETER_ENTITY_REF,
    ET_UNKNOWN_DECL_TYPE,
    ET_WHITESPACE_EXPECTED,

    ET_INVALID_NAMESPACE_URI,
    ET_INVALID_PREFIX,
    ET_INVALID_QUALIFIED_NAME,
    ET_NAMESPACE_URI_NOT_FOUND,
    ET_WRONG_PREFIX_MAPPING_NESTING,

    ET_ENCODING_NOT_SUPPORTED
  );

  TXmlErrorTypes = set of TXmlErrorType;

const
  ET_WARNINGS: TXmlErrorTypes = [
    ET_NONE, // Included in ET_WARNINGS to ease calculations.
    ET_DOUBLE_ATTLISTDECL,
    ET_DOUBLE_ATTDEF,
    ET_DOUBLE_ENTITY_DECL,
    ET_DOUBLE_PARAMETER_ENTITY_DECL,
    ET_UNUSABLE_ENTITY_DECL
  ];

  ET_ERRORS: TXmlErrorTypes = [
    ET_ATTRIBUTE_DEFAULT_TYPE_MISMATCH,
    ET_ATTRIBUTE_DEFINITION_NOT_FOUND,
    ET_ATTRIBUTE_TYPE_MISMATCH,
    ET_DOCTYPE_NOT_FOUND,
    ET_DUPLICATE_ELEMENT_TYPE_DECL,
    ET_DUPLICATE_ENUMERATION_TOKEN,
    ET_DUPLICATE_ID_ON_ELEMENT_TYPE,
    ET_DUPLICATE_ID_VALUE,
    ET_DUPLICATE_NAME_IN_MIXED_CONTENT,
    ET_DUPLICATE_NOTATION_DECL,
    ET_DUPLICATE_NOTATION_ON_ELEMENT_TYPE,
    ET_DUPLICATE_NOTATION_TOKEN,
    ET_DUPLICATE_TOKENS,
    ET_ELEMENT_DECLARED_EMPTY_HAS_CONTENT,
    ET_ELEMENT_TYPE_DECL_NOT_FOUND,
    ET_ELEMENT_WITH_ILLEGAL_ELEMENT_CONTENT,
    ET_ELEMENT_WITH_ILLEGAL_MIXED_CONTENT,
    ET_ENTITY_DECL_NOT_FOUND,
    ET_EXTERNAL_SUBSET_NOT_FOUND,
    ET_FIXED_ATTRIBUTE_MISMATCH,
    ET_ID_NEITHER_IMPLIED_NOR_REQUIRED,
    ET_NONDETERMINISTIC_ELEMENT_CONTENT_MODEL,
    ET_NOTATION_ON_EMPTY_ELEMENT,
    ET_PARAMETER_ENTITY_DECL_NOT_FOUND,
    ET_REQUIRED_ATTRIBUTE_NOT_FOUND,
    ET_TARGET_ID_VALUE_NOT_FOUND,
    ET_TARGET_UNPARSED_ENTITY_NOT_FOUND,
    ET_UNDECLARED_NOTATION_NAME,
    ET_UNRESOLVABLE_ENTITY_REFERENCE,
    ET_UNRESOLVABLE_PARAMETER_ENTITY_REFERENCE,
    ET_WRONG_DECL_OF_PREDEFINED_ENTITY,
    ET_WRONG_ROOT_ELEMENT_TYPE
  ];

  ET_FATAL_ERRORS: TXmlErrorTypes = [
    ET_ATTRIBUTE_VALUE_REFERS_TO_EXTERNAL_ENTITY,
    ET_CDATA_START_EXPECTED,
    ET_COMMENT_START_EXPECTED,
    ET_DOCTYPE_START_EXPECTED,
    ET_DOUBLE_ATTRIBUTE_NAME,
    ET_DOUBLE_EQUALITY_SIGN,
    ET_DOUBLE_HYPHEN_IN_COMMENT,
    ET_DOUBLE_ROOT_ELEMENT,
    ET_HYPHEN_AT_COMMENT_END,
    ET_INVALID_ATTRIBUTE_NAME,
    ET_INVALID_ATTRIBUTE_VALUE,
    ET_INVALID_CDATA_SECTION,
    ET_INVALID_CHARACTER,
    ET_INVALID_CHARREF,
    ET_INVALID_COMMENT,
    ET_INVALID_ELEMENT_NAME,
    ET_INVALID_ENTITY_NAME,
    ET_INVALID_PROCESSING_INSTRUCTION,
    ET_INVALID_PUBID_LITERAL,
    ET_INVALID_SYSTEM_LITERAL,
    ET_INVALID_TEXT_DECL,
    ET_INVALID_XML_DECL,
    ET_LEFT_SQUARE_BRACKET_EXPECTED,
    ET_LT_IN_ATTRIBUTE_VALUE,
    ET_MISSING_ELEMENT_NAME,
    ET_MISSING_ENTITY_NAME,
    ET_MISSING_END_TAG,
    ET_MISSING_EQUALITY_SIGN,
    ET_MISSING_QUOTATION_MARK,
    ET_MISSING_START_TAG,
    ET_MISSING_WHITE_SPACE,
    ET_NOT_IN_ROOT,
    ET_ROOT_NOT_FOUND,
    ET_NO_PROPER_MARKUP_REFERENCED,
    ET_PUBLIC_KEYWORD_EXPECTED,
    ET_QUOTATION_MARK_EXPECTED,
    ET_RECURSIVE_REFERENCE,
    ET_REFERS_TO_UNPARSED_ENTITY,
    ET_SYSTEM_KEYWORD_EXPECTED,
    ET_UNCLOSED_CDATA_SECTION,
    ET_UNCLOSED_CHARREF,
    ET_UNCLOSED_COMMENT,
    ET_UNCLOSED_DOCTYPE,
    ET_UNCLOSED_ELEMENT,
    ET_UNCLOSED_ENTITY_REF,
    ET_UNCLOSED_PROCESSING_INSTRUCTION,
    ET_WRONG_ORDER,

    ET_ATTLIST_DECL_START_EXPECTED,
    ET_INVALID_ATTLIST_DECL_NAME,
    ET_DOUBLE_DOCTYPE,
    ET_CONDITIONAL_SECTION_NOT_ALLOWED,
    ET_ELEMENT_DECL_START_EXPECTED,
    ET_ENTITY_DECL_START_EXPECTED,
    ET_INVALID_ATTRIBUTE_DECL,
    ET_INVALID_CONDITIONAL_SECTION,
    ET_INVALID_DOCTYPE,
    ET_INVALID_ELEMENT_DECL,
    ET_INVALID_ENTITY_DECL,
    ET_INVALID_MARKUP_DECL,
    ET_INVALID_NOTATION_DECL,
    ET_INVALID_PARAMETER_ENTITY_DECL,
    ET_NOTATION_DECL_START_EXPECTED,
    ET_UNCLOSED_ATTLIST_DECL,
    ET_UNCLOSED_CONDITIONAL_SECTION,
    ET_UNCLOSED_ELEMENT_DECL,
    ET_UNCLOSED_ENTITY_DECL,
    ET_UNCLOSED_NOTATION_DECL,
    ET_UNCLOSED_PARAMETER_ENTITY_REF,
    ET_UNKNOWN_DECL_TYPE,
    ET_WHITESPACE_EXPECTED,

    ET_INVALID_NAMESPACE_URI,
    ET_INVALID_PREFIX,
    ET_INVALID_QUALIFIED_NAME,
    ET_NAMESPACE_URI_NOT_FOUND,
    ET_WRONG_PREFIX_MAPPING_NESTING,

    ET_ENCODING_NOT_SUPPORTED
  ];

  ET_DOCTYPE_FATAL_ERRORS: TXmlErrorTypes = [
    ET_ATTLIST_DECL_START_EXPECTED,
    ET_INVALID_ATTLIST_DECL_NAME,
    ET_DOUBLE_DOCTYPE,
    ET_CONDITIONAL_SECTION_NOT_ALLOWED,
    ET_ELEMENT_DECL_START_EXPECTED,
    ET_ENTITY_DECL_START_EXPECTED,
    ET_INVALID_ATTRIBUTE_DECL,
    ET_INVALID_CONDITIONAL_SECTION,
    ET_INVALID_DOCTYPE,
    ET_INVALID_ELEMENT_DECL,
    ET_INVALID_ENTITY_DECL,
    ET_INVALID_MARKUP_DECL,
    ET_INVALID_NOTATION_DECL,
    ET_INVALID_PARAMETER_ENTITY_DECL,
    ET_NOTATION_DECL_START_EXPECTED,
    ET_UNCLOSED_ATTLIST_DECL,
    ET_UNCLOSED_CONDITIONAL_SECTION,
    ET_UNCLOSED_ELEMENT_DECL,
    ET_UNCLOSED_ENTITY_DECL,
    ET_UNCLOSED_NOTATION_DECL,
    ET_UNCLOSED_PARAMETER_ENTITY_REF,
    ET_UNKNOWN_DECL_TYPE,
    ET_WHITESPACE_EXPECTED
  ];

  ET_NAMESPACE_FATAL_ERRORS: TXmlErrorTypes = [
    ET_INVALID_NAMESPACE_URI,
    ET_INVALID_PREFIX,
    ET_INVALID_QUALIFIED_NAME,
    ET_NAMESPACE_URI_NOT_FOUND,
    ET_WRONG_PREFIX_MAPPING_NESTING
  ];

type
  TdomTrinarean = ( T_UNKNOWN,
                    T_TRUE,
                    T_FALSE );

type
  TdomNodeType = ( ntUnknown,
                   ntElement_Node,
                   ntAttribute_Node,
                   ntText_Node,
                   ntCDATA_Section_Node,
                   ntEntity_Reference_Node,
                   ntEntity_Node,
                   ntProcessing_Instruction_Node,
                   ntComment_Node,
                   ntDocument_Node,
                   ntDocument_Type_Node,
                   ntDocument_Fragment_Node,
                   ntNotation_Node,
                   ntDocument_Type_Decl_Node,
                   ntXPath_Namespace_Node );

  TdomWhatToShow = set of TdomNodeType;

const
  SHOW_ALL: TdomWhatToShow = [ ntElement_Node .. High(TDomNodeType) ];
  AS_UNBOUNDED = High(integer);

type
  TdomXPathResultType = ( XPATH_BOOLEAN_TYPE,
                          XPATH_NODE_SET_TYPE,
                          XPATH_NUMBER_TYPE,
                          XPATH_STRING_TYPE );

  TdomXPathResultTypes = set of TdomXPathResultType;

const
  XPATH_ANY_TYPE: TdomXPathResultTypes = [XPATH_BOOLEAN_TYPE .. High(TdomXPathResultType)];

type
  TXmlDataType = (AS_STRING_DATATYPE,
                  AS_NOTATION_DATATYPE,
                  AS_ID_DATATYPE,
                  AS_IDREF_DATATYPE,
                  AS_IDREFS_DATATYPE,
                  AS_ENTITY_DATATYPE,
                  AS_ENTITIES_DATATYPE,
                  AS_NMTOKEN_DATATYPE,
                  AS_NMTOKENS_DATATYPE
{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
                  ,
                  AS_BOOLEAN_DATATYPE,
                  AS_FLOAT_DATATYPE,
                  AS_DOUBLE_DATATYPE,
                  AS_DECIMAL_DATATYPE,
                  AS_HEXBINARY_DATATYPE,
                  AS_BASE64BINARY_DATATYPE,
                  AS_ANYURI_DATATYPE,
                  AS_QNAME_DATATYPE,
                  AS_DURATION_DATATYPE,
                  AS_DATETIME_DATATYPE,
                  AS_DATE_DATATYPE,
                  AS_TIME_DATATYPE,
                  AS_GYEARMONTH_DATATYPE,
                  AS_GYEAR_DATATYPE,
                  AS_GMONTHDAY_DATATYPE,
                  AS_GDAY_DATATYPE,
                  AS_GMONTH_DATATYPE,
                  AS_INTEGER_DATATYPE,
                  AS_NAME_DATATYPE,
                  AS_NCNAME_DATATYPE,
                  AS_NORMALIZEDSTRING_DATATYPE,
                  AS_TOKEN_DATATYPE,
                  AS_LANGUAGE_DATATYPE,
                  AS_NONPOSITIVEINTEGER_DATATYPE,
                  AS_NEGATIVEINTEGER_DATATYPE,
                  AS_LONG_DATATYPE,
                  AS_INT_DATATYPE,
                  AS_SHORT_DATATYPE,
                  AS_BYTE_DATATYPE,
                  AS_NONNEGATIVEINTEGER_DATATYPE,
                  AS_UNSIGNEDLONG_DATATYPE,
                  AS_UNSIGNEDINT_DATATYPE,
                  AS_UNSIGNEDSHORT_DATATYPE,
                  AS_UNSIGNEDBYTE_DATATYPE,
                  AS_POSITIVEINTEGER_DATATYPE,
                  AS_ANYSIMPLETYPE_DATATYPE,
                  AS_ANYTYPE_DATATYPE
{$endif}
                  );

  TdomAttrValueConstraint = (AVC_DEFAULT,
                             AVC_FIXED,
                             AVC_IMPLIED,
                             AVC_REQUIRED);

  TdomASContentModelType= (
{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
                           AS_ALL_CM,
{$endif}
                           AS_CHOICE_CM,
                           AS_ELEMENT_CM,
                           AS_SEQUENCE_CM);

  TdomASContentType = (AS_ANY_CONTENTTYPE,
                       AS_EMPTY_CONTENTTYPE,
                       AS_ELEMENT_CONTENTTYPE,
                       AS_MIXED_CONTENTTYPE,
                       AS_STRICT_MIXED_CONTENTTYPE,
                       AS_UNKNOWN_CONTENTTYPE);

  TdomASEntityType = (AS_INTERNAL_ENTITY,
                      AS_EXTERNAL_ENTITY);

  TdomASEntityUsability = (AS_UNRESOLVED,
                           AS_UNUSABLE,
                           AS_USABLE);

  TdomASFrequency =(AS_REQUIRED_FRQ,
                    AS_OPTIONAL_FRQ,
                    AS_ONE_OR_MORE_FRQ,
                    AS_ZERO_OR_MORE_FRQ);

  TdomASObjectType = (AS_UNDEFINED,
                      AS_ATTRIBUTE_DECLARATION,
                      AS_CONTENT_MODEL,
                      AS_ELEMENT_DECLARATION,
                      AS_ENTITY_DECLARATION,
                      AS_ENTITY_MODEL,
                      AS_NOTATION_DECLARATION);

  TdomASObjectTypeSet = set of TdomASObjectType;

type
  TdomNodeTypeSet = set of TdomNodeType;

  TdomPieceType = ( xmlProcessingInstruction,
                    xmlComment,
                    xmlCDATA,
                    xmlPCDATA,
                    xmlDoctype,
                    xmlStartTag,
                    xmlEndTag,
                    xmlEmptyElementTag,
                    xmlCharRef,
                    xmlEntityRef,
                    xmlParameterEntityRef,
                    xmlEntityDecl,
                    xmlElementDecl,
                    xmlAttributeDecl,
                    xmlNotationDecl,
                    xmlCondSection,
                    xmlParameterEntityDecl,
                    xmlXmlDeclaration,
                    xmlTextDeclaration,
                    xmlUnknown );

  TdomTreePosition = set of ( Tree_Position_Ancestor,
                              Tree_Position_Descendant,
                              Tree_Position_Disconnected,
                              Tree_Position_Equivalent,
                              Tree_Position_Following,
                              Tree_Position_Preceding,
                              Tree_Position_Same_Node );

  TdomEntityResolveOption = (erReplace, erExpand);

  TdomContentspecType = (ctEmpty, ctAny, ctMixed, ctChildren);

  TdomEntityType = (etExternal_Entity, etInternal_Entity);

  TdomFilterResult = (filter_accept, filter_reject, filter_skip);

  TdomFilenameToUriOptions = set of (fuSetLocalhost, fuPlainColon);

  TdomNodeEvent = (neClearing, neRemoving);

  TdomPosition = (posBefore, posAfter);

  TdomStandalone = ( STANDALONE_YES,
                     STANDALONE_NO,
                     STANDALONE_UNSPECIFIED );

  TdomNode               = class;
  TdomAttr               = class;
  TdomElement            = class;
  TdomDocument           = class;
  TdomDocumentType       = class;
  TdomEntity             = class;
  TdomNotation           = class;
  TdomNodeList           = class;

  TdomAbstractView       = class;

  TdomMediaList          = class;

  TXmlSourceCodePiece    = class;

  TXmlStreamBuilder      = class;

  TdomXPathSyntaxNode    = class;
  TXPathExpression       = class;
  TdomXPathCustomResult  = class;
  TdomXPathNodeSetResult = class;

  IDomLocator = interface;

{$ifndef IGNORE_DOCUMENT_FORMAT}
  TdomDocumentClass = class of TdomDocument;

  PdomDocumentFormat = ^TdomDocumentFormat;

  TdomDocumentFormat = record
    DocumentClass: TdomDocumentClass;
    NamespaceUri:  wideString;
    QualifiedName: wideString;
    next:          PdomDocumentFormat;
  end;
{$endif}

  TdomOperationType = ( OT_NODE_ADOPTED,
                        OT_NODE_CLONED,
                        OT_NODE_DESTROYED,
                        OT_NODE_IMPORTED,
                        OT_NODE_RENAMED );

  TdomAttrChange = ( AC_ADDITION,
                     AC_MODIFICATION,
                     AC_REMOVAL );

  TdomXmlnsDeclType = ( NSDT_DEFAULT,
                        NSDT_PREFIXED,
                        NSDT_NONE );

  TdomPreparationStatus = ( PS_UNPREPARED,
                            PS_INCOMPLETE,
                            PS_UNSUCCESSFUL,
                            PS_SUCCESSFUL );

  TdomAutoPrepare = ( AP_NO,
                      AP_INTERNAL_DECLARATIONS,
                      AP_COMPLETE );

  TdomPERefTreatment = ( PT_STOP,
                         PT_SKIP,
                         PT_PARSE );

  TdomUserDataEvent = procedure(const operation: TdomOperationType;
                                const key: WideString;
                                const data: TObject;
                                const src,
                                      dst: TdomNode) of object;

  TdomAttrModifiedEvent = procedure(sender: TObject;
                                    modifiedNode: TdomNode;
                                    attrChange: TdomAttrChange;
                                    relatedAttr: TdomAttr) of object;

  TdomNotifyNodeEvent = procedure(sender: TObject;
                                  node: TdomNode) of object;

  TdomSerializationEvent = procedure(sender: TXmlStreamBuilder;
                                     pieceType: TdomPieceType;
                                     const locator: IDomLocator) of object;

  TdomResolveResourceEvent = procedure(      sender: TObject;
                                       const resourceType,
                                             namespaceUri: wideString;
                                         var publicId,
                                             systemId: wideString;
                                         var stream: TStream;
                                         var certifiedText: boolean) of object;

  TdomError = class;

  TdomErrorEvent = procedure(    sender: TObject;
                                 error: TdomError;
                             var go: boolean) of object;

  TdomErrorNotifyEvent = procedure(sender: TObject;
                                   error: TdomError) of object;

  TdomRequestXPathFunctionResultEvent = procedure(const namespaceUri,
                                                        localName: wideString;
                                                  const contextNode: TdomNode;
                                                  const contextPosition: Integer;
                                                  const contextSize: Integer;
                                                  const arguments: TList;
                                                    var value: TdomXPathCustomResult) of object;

  TdomRequestXPathVariableEvent = procedure(const sender: TXPathExpression;
                                            const namespaceURI,
                                                  localName: wideString;
                                              var value: TdomXPathCustomResult) of object;

  TdomCustomASModelNS     = class;
  TdomASModel             = class;
  TdomASModelCollection   = class;
  TdomASAttributeDecl     = class;
  TdomASElementDecl       = class;
  TdomASEntityDecl        = class;
  TdomASNotationDecl      = class;

  TCustomResourceResolver = class;

{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
  TdomASModelNS           = class;
  TdomASModelCollectionNS = class;
  TdomASAttributeDeclNS   = class;
  TdomASElementDeclNS     = class;
  TdomASEntityDeclNS      = class;
  TdomASNotationDeclNS    = class;
{$endif}

  TDomBaseComponent = class(TComponent)
  protected
    function getXdomVersion: wideString;
  public
    property xdomVersion: wideString read getXdomVersion;
  end;

  TDomImplementation = class(TDomBaseComponent)
  private
  {$ifndef IGNORE_DOCUMENT_FORMAT}
    FDefaultDocumentClass: TdomDocumentClass;
  {$endif}
    FErrorReportLevel: Word;
    FResourceResolver: TCustomResourceResolver;
    FTabWidth: Integer;

    FOnAttrModified: TdomAttrModifiedEvent;
    FOnCharacterDataModified: TdomNotifyNodeEvent;
    FOnError: TdomErrorEvent;
    FOnNodeClearing: TdomNotifyNodeEvent;
    FOnNodeInserted: TdomNotifyNodeEvent;
    FOnNodeRemoving: TdomNotifyNodeEvent;
    FOnRequestXPathFunctionResult: TdomRequestXPathFunctionResultEvent;
    FOnRequestXPathVariable: TdomRequestXPathVariableEvent;

    function getErrorEventsDisabled: boolean;
  protected
{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
    FCreatedASModelsNS: TList;
{$endif}
    FCreatedDocuments: TdomNodeList;
    FCreatedDocumentsListing: TList;
    FCreatedDocumentTypes: TdomNodeList;
    FCreatedDocumentTypesListing: TList;
    procedure doAttrModified(const modifiedNode: TdomNode;
                             const attrChange: TdomAttrChange;
                             const relatedAttr: TdomAttr); virtual;
    procedure doCharacterDataModified(modifiedNode: TdomNode); virtual;
    procedure doError(    sender: TObject;
                          error: TdomError;
                      var go: boolean); virtual;
    procedure doNodeClearing(node: TdomNode); virtual;
    procedure doNodeInserted(node: TdomNode); virtual;
    procedure doNodeRemoving(node: TdomNode); virtual;
    procedure doRequestXPathFunctionResult(const namespaceUri,
                                                 localName: wideString;
                                           const contextNode: TdomNode;
                                           const contextPosition: Integer;
                                           const contextSize: Integer;
                                           const arguments: TList;
                                             var value: TdomXPathCustomResult); virtual;
    procedure doRequestXPathVariable(const XPathExpression: TXPathExpression;
                                     const namespaceURI,
                                           localName: wideString;
                                       var value: TdomXPathCustomResult); virtual;
    function getDocInstance: TdomDocument; virtual; // Derived classes can override this function to make the createDoc() factory method polymorph.
    function getDocumentInstance(const name: wideString;
                                 const doctype: TdomDocumentType): TdomDocument; virtual; // Derived classes can override this function to make the createDocument() factory method polymorph.
    function getDocumentInstanceNS(const aNamespaceUri,
                                         aQualifiedName: wideString;
                                   const doctype: TdomDocumentType): TdomDocument; virtual; // Derived classes can override this function to make the createDocumentNS() factory method polymorph.
    function getDocuments: TdomNodeList; virtual;
    function getDocumentTypes: TdomNodeList; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
{$ifndef IGNORE_DOCUMENT_FORMAT}
    procedure setDefaultDocumentClass(const value: TdomDocumentClass); virtual;
{$endif}
    procedure setResourceResolver(const aResourceResolver: TCustomResourceResolver); virtual;
    procedure setTabWidth(const value: integer); virtual;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure clear; virtual;
{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
    function createASModelNS: TdomASModelNS; virtual;
{$endif}
    function createDoc: TdomDocument; virtual;
    function createDocument(const name: wideString;
                                  doctype: TdomDocumentType): TdomDocument; virtual;
    function createDocumentNS(const namespaceURI,
                                    qualifiedName: wideString;
                                    doctype: TdomDocumentType): TdomDocument; virtual;
    function createDocumentType(const qualifiedName,
                                      publicId,
                                      systemId: wideString): TdomDocumentType; virtual;
    procedure disableErrorEvents; virtual;
    procedure enableErrorEvents; virtual;
{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
    procedure freeASModelNS(var arg: TdomASModelNS); virtual;
{$endif}
    procedure freeDocument(var doc: TdomDocument); virtual;
    procedure freeDocumentType(var docType: TdomDocumentType); virtual;
{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
    procedure freeUnusedASModelsNS; virtual;
{$endif}
    function hasFeature(const feature,
                              version: wideString): boolean; virtual;
{$ifndef IGNORE_DOCUMENT_FORMAT}
    function getDocumentClass(const aNamespaceUri,
                                    aQualifiedName: wideString): TdomDocumentClass; virtual;
{$endif}
    function handleError(const sender: TObject;
                         const error: TdomError): boolean; virtual;
{$ifndef IGNORE_DOCUMENT_FORMAT}
    class procedure registerDocumentFormat(const aNamespaceUri,
                                                 aQualifiedName: wideString;
                                                 aDocumentClass: TdomDocumentClass); virtual;
{$endif}
    function resolveResource(const aBaseURI: wideString;
                               var publicId,
                                   systemId: wideString): TStream; virtual;
{$ifndef IGNORE_DOCUMENT_FORMAT}
    function supportsDocumentFormat(const aNamespaceUri,
                                          aQualifiedName: wideString): boolean; virtual;
    class procedure unregisterDocumentClass(const aDocumentClass: TdomDocumentClass); virtual;

{$endif}
{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
    property ASModelsNS: TList read FCreatedASModelsNS;
{$endif}
{$ifndef IGNORE_DOCUMENT_FORMAT}
    property defaultDocumentClass: TdomDocumentClass read FDefaultDocumentClass write setDefaultDocumentClass;
{$endif}
    property documents: TdomNodeList read getDocuments;
    property documentTypes: TdomNodeList read getDocumentTypes;
    property errorEventsDisabled: boolean read getErrorEventsDisabled;
  published
    property ResourceResolver: TCustomResourceResolver read FResourceResolver write setResourceResolver;
    property TabWidth: Integer read FTabWidth write setTabWidth default 4;

    property OnAttrModified:          TdomAttrModifiedEvent read FOnAttrModified write FOnAttrModified;
    property OnCharacterDataModified: TdomNotifyNodeEvent read FOnCharacterDataModified write FOnCharacterDataModified;
    property OnError:                 TdomErrorEvent read FOnError write FOnError;
    property OnNodeClearing:          TdomNotifyNodeEvent read FOnNodeClearing write FOnNodeClearing;
    property OnNodeInserted:          TdomNotifyNodeEvent read FOnNodeInserted write FOnNodeInserted;
    property OnNodeRemoving:          TdomNotifyNodeEvent read FOnNodeRemoving write FOnNodeRemoving;
    property OnRequestXPathFunctionResult:  TdomRequestXPathFunctionResultEvent read FOnRequestXPathFunctionResult write FOnRequestXPathFunctionResult;
    property OnRequestXPathVariable:  TdomRequestXPathVariableEvent read FOnRequestXPathVariable write FOnRequestXPathVariable;
  end;

  TdomNodeFilter = class
  public
    function acceptNode(const n: TdomNode): TdomFilterResult; virtual; abstract;
  end;

  TdomTreeWalker = class
  private
    FCurrentNode: TdomNode;
    FExpandEntityReferences: boolean;
    FFilter: TdomNodeFilter;
    FRoot: TdomNode;
    FWhatToShow: TdomWhatToShow;
  protected
    function findFirstChild(const oldNode: TdomNode): TdomNode; virtual;
    function findLastChild(const oldNode: TdomNode): TdomNode; virtual;
    function findNextNode(oldNode: TdomNode): TdomNode; virtual;
    function findNextSibling(const oldNode: TdomNode): TdomNode; virtual;
    function findParentNode(const oldNode: TdomNode): TdomNode; virtual;
    function findPreviousNode(const oldNode: TdomNode): TdomNode; virtual;
    function findPreviousSibling(const oldNode: TdomNode): TdomNode; virtual;
    procedure setCurrentNode(const node: TdomNode); virtual;
    procedure setExpandEntityReferences(const value: boolean); virtual;  // Derived classes may move this method to the public section to allow write access.
    procedure setFilter(const value: TdomNodeFilter); virtual;           // Derived classes may move this method to the public section to allow write access.
    procedure setRoot(const node: TdomNode); virtual;                    // Derived classes may move this method to the public section to allow write access.
    procedure setWhatToShow(const value: TdomWhatToShow); virtual;       // Derived classes may move this method to the public section to allow write access.
  public
    constructor create(const root: TdomNode;
                       const whatToShow: TdomWhatToShow;
                       const nodeFilter: TdomNodeFilter;
                       const entityReferenceExpansion: boolean); virtual;
    function parentNode: TdomNode; virtual;
    function firstChild: TdomNode; virtual;
    function lastChild: TdomNode; virtual;
    function previousSibling: TdomNode; virtual;
    function nextSibling: TdomNode; virtual;
    function nextNode: TdomNode; virtual;
    function previousNode: TdomNode; virtual;
    property currentNode: TdomNode read FCurrentNode write setCurrentNode;
    property expandEntityReferences: boolean read FExpandEntityReferences;
    property filter: TdomNodeFilter read FFilter;
    property root: TdomNode read FRoot;
    property whatToShow: TdomWhatToShow read FWhatToShow;
  end;

  TdomNodeIterator = class
  private
    FRoot: TdomNode;
    FReferenceNode: TdomNode;
    FPosition: TdomPosition; // Position of the Iterator relativ to FReferenceNode
    FWhatToShow: TdomWhatToShow;
    FExpandEntityReferences: boolean;
    FFilter: TdomNodeFilter;
    FInvalid: boolean;
  protected
    procedure handleNodeEvent(const node: TdomNode;
                              const eventType: TdomNodeEvent); virtual; // Used to recieve notifications about node events.
    function findNextNode(oldNode: TdomNode): TdomNode; virtual;
    function findPreviousNode(const oldNode: TdomNode): TdomNode; virtual;
  public
    constructor create(const root: TdomNode;
                       const whatToShow: TdomWhatToShow;
                       const nodeFilter: TdomNodeFilter;
                       const entityReferenceExpansion: boolean); virtual;
    procedure detach; virtual;
    function nextNode: TdomNode; virtual;
    function previousNode: TdomNode; virtual;
    property expandEntityReferences: boolean read FExpandEntityReferences;
    property filter: TdomNodeFilter read FFilter;
    property root: TdomNode read FRoot;
    property whatToShow: TdomWhatToShow read FWhatToShow;
  end;

  TdomNodeList = class
  private
    FNodeList: TList;
  protected
    function getLength: integer; virtual;
    function indexOf(const node: TdomNode): integer; virtual;
  public
    constructor create(const nodeList: TList);
    function item(const index: integer): TdomNode; virtual;
    property length: integer read getLength;
  end;

  TdomElementsNodeList = class(TdomNodeList)
  private
    FQueryName: wideString;
    FStartElement: TdomNode;
  protected
    function getLength: integer; override;
  public
    function indexOf(const node: TdomNode): integer; override;
    function item(const index: integer): TdomNode; override;
    constructor create(const queryName: wideString;
                       const startElement: TdomNode); virtual;
  end;

  TdomElementsNodeListNS = class(TdomNodeList)
  private
    FQueryNamespaceURI: wideString;
    FQueryLocalName: wideString;
    FStartElement: TdomNode;
  protected
    function getLength: integer; override;
  public
    function indexOf(const node: TdomNode): integer; override;
    function item(const index: integer): TdomNode; override;
    constructor create(const queryNamespaceURI,
                             queryLocalName: wideString;
                       const startElement: TdomNode); virtual;
  end;

  TdomCustomNode = class(TCustomOwnedNode)
  protected
    function getNodeName: wideString; virtual; abstract;
    procedure raiseException(const E: ExceptClass); override;
  public
    property nodeName: wideString read getNodeName;
  end;

  TdomCustomNodeClass = class of TdomCustomNode;

  TdomOwnerNamedNodeMap = class(TPersistent)
  private
    FItemClass: TdomCustomNodeClass;
    FNodeList: TUtilsWideStringList;
  protected
    function getCount: integer; virtual;
    function getItems(index: integer): TdomCustomNode; virtual;
  public
    constructor create(const aItemClass: TdomCustomNodeClass);
    destructor destroy; override;
    function add(const node: TdomCustomNode): integer; virtual;
    procedure clear; virtual;
    procedure Delete(const index: integer); virtual;
    function extractItem(const node: TdomCustomNode): TdomCustomNode; virtual;
    function getNamedItem(const name: wideString): TdomCustomNode; virtual;
    function hasNamedItem(const name: wideString): boolean; virtual;
    function indexOfItem(const node: TdomCustomNode): integer; virtual;
    function indexOfNamedItem(const name: wideString): integer; virtual;
    function removeItem(const node: TdomCustomNode): integer; virtual;
    function removeNamedItem(const name: wideString): integer; virtual;
    property itemClass: TdomCustomNodeClass read FItemClass;
    property items[index: integer]: TdomCustomNode read getItems; default;
    property count: integer read getCount;
  end;

  TdomNamedNodeMap = class(TdomNodeList)
  private
    FAllowedNodeTypes: TDomNodeTypeSet;
    FDefaultNamespaceAware: boolean;
    FOwnerNode: TdomNode;
  protected
    procedure checkAllowedNodeType(const node: TdomNode);
    procedure checkHasNode(const node: TdomNode);
    procedure checkNamespaceAware;
    procedure checkNotInUse(const node: TdomNode);
    procedure checkNotNamespaceAware;
    procedure checkNotReadOnly;
    procedure checkSameReferenceDocument(const node: TdomNode);
    function getNamespaceAware: boolean;
    function getReadOnly: boolean;
    procedure internalAdd(const node: TdomNode); virtual;
    procedure internalRemove(const node: TdomNode); virtual;
    function removeItem(const arg: TdomNode): TdomNode; virtual;
  public
    constructor create(const aOwner: TdomNode;
                       const nodeList: TList;
                       const allowedNTs: TDomNodeTypeSet;
                       const defaultNamespaceAware: boolean); virtual;
    function getNamedItem(const name: wideString): TdomNode; virtual;
    function getNamedItemNS(const namespaceURI,
                                  localName: wideString): TdomNode; virtual;
    function removeNamedItem(const name: wideString): TdomNode; virtual;
    function removeNamedItemNS(const namespaceURI,
                                     localName: wideString): TdomNode; virtual;
    function setNamedItem(const arg: TdomNode): TdomNode; virtual;
    function setNamedItemNS(const arg: TdomNode): TdomNode; virtual;
    property namespaceAware: boolean read GetNamespaceAware;
    property ownerNode: TdomNode read FOwnerNode;
    property readOnly: boolean read getReadOnly;
  end;

  TdomNode = class (TdomCustomNode)
  private
    FIsNamespaceNode: boolean;
    FNodeList: TdomNodeList;
    FNodeValue: wideString;
    FOwnerDocument: TdomDocument;
    FUserData: TUtilsWideStringList;
    FUserDataHandlers: TList;
    procedure makeChildrenReadonly;
    function previousNode: TdomNode;
    function hasEntRef(const entName: widestring): boolean;
  protected
    FAllowedChildTypes: set of TDomNodeType;
    procedure checkTypeAllowed(const node: TdomNode); virtual;
    procedure doAfterAddition(const node: TCustomOwnedNode); override;
    procedure doBeforeClear; override;
    procedure doBeforeRemoval(const node: TCustomOwnedNode); override;
    function getAbsoluteIndex: integer; virtual;
    function getAttributes: TdomNamedNodeMap; virtual;
    function getBaseUri: wideString; virtual;
    function getChildNodes: TdomNodeList; virtual;
    function getDocument: TdomDocument; virtual;
    function getExpandedName: wideString; virtual;
    function getFirstChild: TdomNode; reintroduce; virtual;
    function getLanguage: wideString; virtual;
    function getLastChild: TdomNode; reintroduce; virtual;
    function getLevel: integer; virtual;
    function getLocalName: wideString; virtual;
    function getNamespaceURI: wideString; virtual;
    function getNextSibling: TdomNode; reintroduce; virtual;
    function getNodeName: wideString; override;
    function getNodeValue: wideString; virtual;
    function getNodeType: TdomNodeType; virtual;
    function getParentNode: TdomNode; virtual;
    function getPreviousSibling: TdomNode; reintroduce; virtual;
    function getPrefix: wideString; virtual;
    function getReferenceDocument: TdomDocument; virtual;
    function getTabWidth: integer; virtual;
    function getTextContent: wideString; virtual;
    function getXPathStringValue: wideString; virtual;
    function sendErrorNotification(const xmlErrorType: TXmlErrorType;
                                   const relNode: TdomNode): boolean; virtual;
    procedure setNodeValue(const value: wideString); virtual;
    procedure setPrefix(const value: wideString); virtual;
    function validate2: boolean; virtual;
    function validateIDREFS: boolean; virtual;
  public
    constructor create(const aOwner: TCustomOwnedObject);
    destructor destroy; override;
    function  appendChild(const newChild: TdomNode): TdomNode; virtual;
    procedure clear; override;
    function  cloneNode(const deep: boolean): TdomNode; virtual;
    function  compareTreePosition(const other: TdomNode): TdomTreePosition; virtual;
    function  evaluateToBoolean(const expression: wideString): boolean; virtual;
    function  evaluateToNumber(const expression: wideString): double; virtual;
    function  evaluateToNode(const expression: wideString): TdomNode; virtual;
    function  evaluateToWideString(const expression: wideString): wideString; virtual;
    function  findFirstChildElement: TdomElement; virtual;
    function  findLastChildElement: TdomElement; virtual;
    function  findNextSiblingElement: TdomElement; virtual;
    function  findParentElement: TdomElement; virtual;
    function  findPreviousSiblingElement: TdomElement; virtual;
    function  getFirstChildElement(const name: wideString): TdomElement; virtual;
    function  getFirstChildElementNS(const namespaceURI,
                                           localName: wideString): TdomElement; virtual;
    function  getLastChildElement(const name: wideString): TdomElement; virtual;
    function  getLastChildElementNS(const namespaceURI,
                                          localName: wideString): TdomElement; virtual;
    function  getNextSiblingElement(const name: wideString): TdomElement; virtual;
    function  getNextSiblingElementNS(const namespaceURI,
                                            localName: wideString): TdomElement; virtual;
    function  getParentElement(const name: wideString): TdomElement; virtual;
    function  getParentElementNS(const namespaceURI,
                                       localName: wideString): TdomElement; virtual;
    function  getPreviousSiblingElement(const name: wideString): TdomElement; virtual;
    function  getPreviousSiblingElementNS(const namespaceURI,
                                                localName: wideString): TdomElement; virtual;
    function  getUserData(const key: wideString): TObject; virtual;
    function  hasAsAncestor(const node: TdomNode): boolean; reintroduce; virtual;
    function  hasAttributes: boolean; virtual;
    function  hasChildNodes: boolean; virtual;
    function  insertBefore(const newChild,
                                 refChild: TdomNode): TdomNode; reintroduce; virtual;
    function  lookupNamespaceURI(const aPrefix: wideString): wideString; virtual;
    procedure normalize; virtual;
    function  removeChild(const oldChild: TdomNode): TdomNode; virtual;
    function  replaceChild(const newChild,
                                 oldChild: TdomNode): TdomNode; virtual;
    function  resolveEntityReferences(const opt: TdomEntityResolveOption): integer; virtual;
    function  setUserData(const key: wideString;
                          const data: TObject;
                          const handler: TdomUserDataEvent): TObject; virtual;
    function  supports(const feature,
                             version: wideString): boolean; virtual;

    property absoluteIndex:     integer          read getAbsoluteIndex;
    property attributes:        TdomNamedNodeMap read getAttributes;
    property baseUri:           wideString       read getBaseUri;
    property childNodes:        TdomNodeList     read getChildNodes;
    property expandedName:      wideString       read getExpandedName;
    property firstChild:        TdomNode         read getFirstChild;
    property isNamespaceNode:   boolean          read FIsNamespaceNode;
    property isReadonly:        boolean          read getReadOnly;
    property language:          wideString       read getLanguage;
    property lastChild:         TdomNode         read getLastChild;
    property level:             integer          read getLevel;
    property localName:         wideString       read getLocalName;
    property namespaceURI:      wideString       read getNamespaceURI;
    property nextSibling:       TdomNode         read getNextSibling;
    property nodeType:          TdomNodeType     read getNodeType;
    property nodeValue:         wideString       read getNodeValue write setNodeValue;
    property ownerDocument:     TdomDocument     read getDocument;
    property parentNode:        TdomNode         read getParentNode;
    property previousSibling:   TdomNode         read getPreviousSibling;
    property prefix:            wideString       read getPrefix    write setPrefix;
    property referenceDocument: TdomDocument     read getReferenceDocument;  // xxx rename to parentDocument?
    property textContent:       wideString       read getTextContent;
    property XPathStringValue:  wideString       read getXPathStringValue;
  end;

  TdomCharacterData = class (TdomNode)
  private
    function getData: wideString; virtual;
    procedure setData(const Value: wideString); virtual;
    function getLength: integer; virtual;
  protected
    procedure doCharacterDataModified; virtual;
  public
    constructor create(const aOwner: TdomDocument);
    function substringData(const offset,
                                 count: integer): wideString; virtual;
    procedure appendData(const arg: wideString); virtual;
    procedure insertData(const offset: integer;
                         const arg: wideString); virtual;
    procedure deleteData(const offset,
                               count: integer); virtual;
    procedure replaceData(const offset,
                                count: integer;
                          const arg: wideString); virtual;
    property data: wideString read getData write setData;
    property length: integer read getLength;
  end;

  TdomAttr = class (TdomNode)
  private
    FIsXmlnsDecl: TdomXmlnsDeclType;
    FLocalName: wideString;
    FNamespaceURI: wideString;
    FNodeName: wideString;
    FOwnerMap: TdomNamedNodeMap;
    FPrefix: wideString;
  protected
    FSpecified: boolean;
    procedure doAttrModified(const attrChange: TdomAttrChange); virtual;
    function getExpandedName: wideString; override;
    function getIsId: boolean; virtual;
    function getIsXmlnsDecl: TdomXmlnsDeclType; virtual;
    function getLocalName: wideString; override;
    function getName: wideString; virtual;
    function getNamespaceURI: wideString; override;
    function getNextSibling: TdomNode; override;
    function getNodeName: wideString; override;
    function getNodeType: TdomNodeType; override;
    function getOwnerElement: TdomElement; virtual;
    function getPrefix: wideString; override;
    function getPreviousSibling: TdomNode; override;
    function getSpecified: boolean; virtual;
    function getValue: wideString; virtual;
    procedure setNodeValue(const value: wideString); override;
    procedure setPrefix(const value: wideString); override;
    function validateIDREFS: boolean; override;
  public
    constructor create(const aOwner: TdomDocument;
                       const name: wideString;
                       const spcfd: boolean);
    constructor createNS(const aOwner: TdomDocument;
                         const namespaceURI,
                               qualifiedName: wideString;
                         const spcfd: boolean);
    destructor destroy; override;
    function lookupNamespaceURI(const aPrefix: wideString): wideString; override;
    property isId: boolean read getIsId;
    property isXmlnsDecl: TdomXmlnsDeclType read getIsXmlnsDecl;
    property name: wideString read getName;
    property ownerElement: TdomElement read getOwnerElement;
    property specified: boolean read getSpecified;
    property value: wideString read getValue;
  end;

  TdomElement = class (TdomNode)
  private
    FAttributeList: TdomNamedNodeMap;
    FAttributeListing: TList;
    FCreatedElementsNodeListNSs: TList;
    FCreatedElementsNodeLists: TList;
    FLocalName: wideString;
    FNamespaceURI: wideString;
    FNodeName: wideString;
    FPrefix: wideString;
  protected
    procedure doAttrModified(const originalTarget: TdomNode;
                             const attrChange: TdomAttrChange;
                             const relatedAttr: TdomAttr); virtual;
    procedure doBeforeClear; override;
    function getExpandedName: wideString; override;
    function getLocalName: wideString; override;
    function getNamespaceURI: wideString; override;
    function getNodeName: wideString; override;
    function getNodeType: TdomNodeType; override;
    function getPrefix: wideString; override;
    procedure setNodeValue(const value: wideString); override;
    procedure setPrefix(const value: wideString); override;
    function validate2: boolean; override;
    function validateIDREFS: boolean; override;
  public
    constructor create(const aOwner: TdomDocument;
                       const tagName: wideString);
    constructor createNS(const aOwner: TdomDocument;
                         const namespaceURI,
                               qualifiedName: wideString);
    destructor destroy; override;
    function getAttributeLiteralValue(const name: wideString): wideString; virtual;
    function getAttributeNode(const name: wideString): TdomAttr; virtual;
    function getAttributeNodeNS(const namespaceURI,
                                      localName: wideString): TdomAttr; virtual;
    function getAttributeNormalizedValue(const name: wideString): wideString; virtual;
    function getAttributeNSLiteralValue(const namespaceURI,
                                              localName: wideString): wideString; virtual;
    function getAttributeNSNormalizedValue(const namespaceURI,
                                                 localName: wideString): wideString; virtual;
    function getAttributes: TdomNamedNodeMap; override;
    function getElementsByTagName(const name: wideString): TdomNodeList; virtual;
    function getElementsByTagNameNS(const namespaceURI,
                                          localName: wideString): TdomNodeList; virtual;
    function getTagName: wideString; virtual;
    function hasAttribute(const name: wideString): boolean; virtual;
    function hasAttributeNS(const namespaceURI,
                                  localName: wideString): boolean; virtual;
    function lookupNamespaceURI(const aPrefix: wideString): wideString; override;
    procedure normalize; override;
    function removeAttribute(const name: wideString): TdomAttr; virtual;
    function removeAttributeNode(const oldAttr: TdomAttr): TdomAttr; virtual;
    function removeAttributeNS(const namespaceURI,
                                     localName: wideString): TdomAttr; virtual;
    function resolveEntityReferences(const opt: TdomEntityResolveOption): integer; override;
    function setAttribute(const name,
                                value: wideString): TdomAttr; virtual;
    function setAttributeNode(const newAttr: TdomAttr): TdomAttr; virtual;
    function setAttributeNodeNS(const newAttr: TdomAttr): TdomAttr; virtual;
    function setAttributeNS(const namespaceURI,
                                  qualifiedName,
                                  value: wideString): TdomAttr; virtual;
    property tagName: wideString read getTagName;
  end;

  TdomText = class (TdomCharacterData)
  protected
    function getIsWhitespaceInElementContent: boolean; virtual;
    function getNodeName: wideString; override;
    function getNodeType: TdomNodeType; override;
  public
    constructor create(const aOwner: TdomDocument);
    function splitText(const offset: integer): TdomText; virtual;
    property isWhitespaceInElementContent: boolean read getIsWhitespaceInElementContent;
  end;

  TdomComment = class (TdomCharacterData)
  protected
    function getNodeName: wideString; override;
    function getNodeType: TdomNodeType; override;
  public
    constructor create(const aOwner: TdomDocument);
  end;

  TdomProcessingInstruction = class (TdomNode)
  private
    FTarget: wideString;
  protected
    procedure doCharacterDataModified; virtual;
    function getData: wideString; virtual;
    function getExpandedName: wideString; override;
    function getNodeName: wideString; override;
    function getNodeType: TdomNodeType; override;
    procedure setData(const value: wideString); virtual;
  public
    constructor create(const aOwner: TdomDocument;
                       const targ: wideString);
    property target: wideString read FTarget;
    property data: wideString read getData write setData;
  end;

  TdomCDATASection = class (TdomText)
  protected
    function getNodeName: wideString; override;
    function getNodeType: TdomNodeType; override;
  public
    constructor create(const aOwner: TdomDocument);
  end;

  TdomDocumentTypeDecl = class (TdomNode)
  private
    FDtdModel: TdomASModelCollection;
    FInternalSubset: wideString;
    FNodeName: wideString;
    FPreparationStatus: TdomPreparationStatus;
    FPublicId: wideString;
    FSystemId: wideString;
  protected
    function getInternalSubset: wideString; virtual;
    function getName: wideString; virtual;
    function getNodeName: wideString; override;
    function getNodeType: TdomNodeType; override;
    function getPublicId: wideString; virtual;
    function getSystemId: wideString; virtual;
    procedure setNodeValue(const value: wideString); override;

    property dtdModel: TdomASModelCollection read FDtdModel;
  public
    constructor create(const aOwner: TdomDocument;
                       const doctypeName,
                             pubId,
                             sysId,
                             intSubset: wideString);
    procedure clear; override;
    function prepare(const stopAtExtDecl: boolean;
                     const intSubsetStartByteNumber,
                           intSubsetStartCharNumber,
                           intSubsetStartColumn,
                           intSubsetStartLine: Int64): boolean; virtual;

    property internalSubset: wideString read getInternalSubset;
    property name: wideString read getName;
    property preparationStatus: TdomPreparationStatus read FPreparationStatus;
    property publicId: wideString read getPublicId;
    property systemId: wideString read getSystemId;
  end;

  TdomDocumentType = class (TdomNode)
  private
    FDomImpl: TDomImplementation;
    FEntitiesListing: TList;
    FEntitiesList: TdomNamedNodeMap;
    FNodeName: wideString;
    FNotationsList: TdomNamedNodeMap;
    FNotationsListing: TList;
    FPublicId: wideString;
    FSystemId: wideString;
    procedure attachOwnerDocument(const doc: TdomDocument);
    procedure detachOwnerDocument;
  protected
    function getEntities: TdomNamedNodeMap; virtual;
    function getInternalSubset: wideString; virtual;
    function getName: wideString; virtual;
    function getNodeName: wideString; override;
    function getNodeType: TdomNodeType; override;
    function getNotations: TdomNamedNodeMap; virtual;
    function getPublicId: wideString; virtual;
    function getReadOnly: boolean; override;
    function getSystemId: wideString; virtual;
    procedure setNodeValue(const value: wideString); override;
  public
    constructor create(const aOwner: TDomImplementation;
                       const doctypeName,
                             pubId,
                             sysId: wideString);
    destructor  destroy; override;

    property domImplementation: TDomImplementation read FDomImpl;
    property entities: TdomNamedNodeMap read getEntities;
    property internalSubset: wideString read getInternalSubset;
    property name: wideString read getName;
    property notations: TdomNamedNodeMap read getNotations;
    property publicId: wideString read getPublicId;
    property systemId: wideString read getSystemId;
  end;

  TdomNotation = class (TdomNode)
  private
    FNodeName: wideString;
    FPublicId: wideString;
    FSystemId: wideString;
  protected
    function getNodeName: wideString; override;
    function getNodeType: TdomNodeType; override;
    procedure setNodeValue(const value: wideString); override;
  public
    constructor create(const aOwner: TdomDocument;
                       const name,
                             pubId,
                             sysId: wideString);
    property publicId: wideString read FPublicId;
    property systemId: wideString read FSystemId;
  end;

  TdomEntity = class (TdomNode)
  private
    FInputEncoding: wideString;
    FNodeName: wideString;
    FNotationName: wideString;
    FPublicId: wideString;
    FSystemId: wideString;
    FXmlEncoding: wideString;
    FXmlVersion: wideString;
  protected
    function getNodeName: wideString; override;
    function getNodeType: TdomNodeType; override;
    function getNotationName: wideString; virtual;
    procedure setNodeValue(const value: wideString); override;
  public
    constructor create(const aOwner: TdomDocument;
                       const name,
                             pubId,
                             sysId,
                             notaName: wideString);
    function insertBefore(const newChild,
                                refChild: TdomNode): TdomNode; override;
    function replaceChild(const newChild,
                                oldChild: TdomNode): TdomNode; override;
    function appendChild(const newChild: TdomNode): TdomNode; override;

    property inputEncoding: wideString read FInputEncoding write FInputEncoding;
    property notationName: wideString read getNotationName;
    property publicId: wideString read FPublicId;
    property systemId: wideString read FSystemId;
    property xmlEncoding: wideString read FXmlEncoding write FXmlEncoding;
    property xmlVersion: wideString read FXmlVersion write FXmlVersion;
  end;

  TdomEntityReference = class (TdomNode)
  private
    FNodeName: wideString;
  protected
    function expand: boolean; virtual;
    function getNodeName: wideString; override;
    function getNodeType: TdomNodeType; override;
    procedure setNodeValue(const value: wideString); override;
    function validate2: boolean; override;
  public
    constructor create(const aOwner: TdomDocument;
                       const name: wideString);
    function cloneNode(const deep: boolean): TdomNode; override;
  end;

  TdomDocumentFragment = class (TdomNode)
  protected
    function getAbsoluteIndex: integer; override;
    function getLevel: integer; override;
    function getNodeName: wideString; override;
    function getNodeType: TdomNodeType; override;
    procedure setNodeValue(const value: wideString); override;
  public
    constructor create(const aOwner: TdomDocument); virtual;
    function resolveEntityReferences(const opt: TdomEntityResolveOption): integer; override;
  end;

  TdomXPathNamespace = class (TdomNode)
  private
    FNamespaceURI: wideString;
    FOwnerElement: TdomElement;
    FPrefix: wideString;
    function getOwnerSet: TdomXPathNodeSetResult;
  protected
    function getDocument: TdomDocument; override;
    function getExpandedName: wideString; override;
    function getLocalName: wideString; override;
    function getNamespaceURI: wideString; override;
    function getNodeName: wideString; override;
    function getNodeType: TdomNodeType; override;
    function getNodeValue: wideString; override;
    function getOwnerElement: TdomElement; virtual;
    function getPrefix: wideString; override;
  public
    constructor create(const aOwnerSet: TdomXPathNodeSetResult;
                       const aOwnerElement: TdomElement;
                       const aNamespaceUri,
                             aPrefix: wideString);
    property ownerElement: TdomElement read getOwnerElement;
    property ownerSet: TdomXPathNodeSetResult read getOwnerSet;
    function lookupNamespaceURI(const aPrefix: wideString): wideString; override;
  end;

  TdomDocument = class (TdomNode)
  private
{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
    FASModelsNS: TdomASModelCollectionNS;
{$endif}
    FCreatedNodeIterators: TList;
    FCreatedTreeWalkers: TList;
    FCreatedElementsNodeLists: TList;
    FCreatedElementsNodeListNSs: TList;
    FDefaultView: TdomAbstractView;
    FDoctype: TdomDocumentType;
    FDocumentUri: wideString;
    FDOMImpl: TDomImplementation;
    FIDs: TUtilsWideStringList;
    FInputEncoding: wideString;
    FModified: boolean;
    FSystemId: wideString;
    FXmlEncoding: wideString;
    FXmlStandalone: TdomStandalone;
    FXmlVersion: wideString;

    FOnAttrModified: TdomAttrModifiedEvent;
    FOnCharacterDataModified: TdomNotifyNodeEvent;
    FOnNodeClearing: TdomNotifyNodeEvent;
    FOnNodeInserted: TdomNotifyNodeEvent;
    FOnNodeRemoving: TdomNotifyNodeEvent;

    function getDtdModel: TdomASModelCollection;
    procedure notifyIterators(const node: TdomNode;
                              const eventType: TdomNodeEvent);
    function validateAttr(const node: TdomAttr): TXmlErrorType;
    function validateElement(const node: TdomElement): TXmlErrorType;
    function validateEntityRef(const node: TdomEntityReference): TXmlErrorType;
  protected
    function calculateNormalizedAttrValue(const Attr: TdomAttr): wideString; virtual;
    function createEntity(const name,
                                pubId,
                                sysId,
                                notaName: wideString): TdomEntity; virtual;
    function createNotation(const name,
                                  pubId,
                                  sysId: wideString): TdomNotation; virtual;
    procedure doAttrModified(const sourceNode: TdomNode;
                             const attrChange: TdomAttrChange;
                             const relatedAttr: TdomAttr); virtual;
    procedure doBeforeClear; override;
    procedure doCharacterDataModified(node: TdomNode); virtual;
    procedure doNodeClearing(node: TdomNode); virtual;
    procedure doNodeInserted(node: TdomNode); virtual;
    procedure doNodeRemoving(node: TdomNode); virtual;
    function getAbsoluteIndex: integer; override;
    function getBaseUri: wideString; override;
    function getDoctypeDecl: TdomDocumentTypeDecl; virtual;
    function getDocumentElement: TdomElement; virtual;
    function getLevel: integer; override;
    function getNodeName: wideString; override;
    function getNodeType: TdomNodeType; override;
    function getReferenceDocument: TdomDocument; override;
    function importNode2(const importedNode: TdomNode;
                         const deep: boolean): TdomNode; virtual;
    procedure initDoc(const tagName: wideString); virtual;
    procedure initDocNS(const namespaceURI,
                              qualifiedName: wideString); virtual;
    function precalculateNormalizedAttrValue(const S: wideString): wideString; virtual;
    function prepareAttributes2(const node: TdomNode): boolean; virtual;
    procedure setNodeValue(const value: wideString); override;
    function validateIDREFS: boolean; override;

    function validateDefaultAttr(const node: TdomElement): TXmlErrorType; virtual;
    function validateNode(const node: TdomNode): TXmlErrorType; virtual;

    // Validation support functions:
    // xxx These functions are likely to be removed from a future XDOM version.
    function getAttrType(const elementType,
                               attributeName: wideString): TXmlDataType; virtual;
    function getContentType(const elementType: wideString): TdomASContentType; virtual;
    procedure getReplacementText(const entityName: wideString;
                                   out replText: wideString;
                                   out error: TXmlErrorType); virtual;
    function hasAttrDef(const elementType,
                              attributeName: wideString): boolean; virtual;
    function hasAttrEnum(const elementType,
                               attributeName,
                               attributeValue: wideString): boolean; virtual;
    function hasUnparsedEntity(const entityName: wideString): boolean; virtual;
    function refersToExternalEntity(const S: wideString): boolean; virtual;
    function refersToLTEntity(const S: wideString): boolean; virtual;

    // Properties:
    property DtdModel: TdomASModelCollection read getDtdModel;
    property IDs: TUtilsWideStringList read FIDs;
  public
    constructor create(const aOwner: TDomImplementation;
                       const aDoctype: TdomDocumentType);
    destructor destroy; override;
    function appendChild(const newChild: TdomNode): TdomNode; override;
    procedure clearInvalidNodeIterators; virtual;
    function createAttribute(const name: wideString): TdomAttr; virtual;
    function createAttributeNS(const namespaceURI,
                                     qualifiedName: wideString): TdomAttr; virtual;
    function createCDATASection(const data: wideString): TdomCDATASection; virtual;
    function createComment(const data: wideString): TdomComment; virtual;
    function createDocumentFragment: TdomDocumentFragment; virtual;
    function createDocumentTypeDecl(const name,
                                          pubId,
                                          sysId,
                                          intSubset: wideString): TdomDocumentTypeDecl; virtual;
    function createElement(const tagName: wideString): TdomElement; virtual;
    function createElementNS(const namespaceURI,
                                   qualifiedName: wideString): TdomElement; virtual;
    function createEntityReference(const name: wideString): TdomEntityReference; virtual;
    function createNodeIterator(const root: TdomNode;
                                      whatToShow: TdomWhatToShow;
                                      nodeFilter: TdomNodeFilter;
                                      entityReferenceExpansion: boolean): TdomNodeIterator; virtual;
    function createProcessingInstruction(const targ,
                                               data : wideString): TdomProcessingInstruction; virtual;
    function createTextNode(const data: wideString): TdomText; virtual;
    function createTreeWalker(const root: TdomNode;
                                    whatToShow: TdomWhatToShow;
                                    nodeFilter: TdomNodeFilter;
                                    entityReferenceExpansion: boolean): TdomTreeWalker; virtual;
    procedure freeTreeWalker(var treeWalker: TdomTreeWalker); virtual;
    function getElementById(const elementId: wideString): TdomElement; virtual;
    function getElementsByTagName(const tagName: wideString): TdomNodeList; virtual;
    function getElementsByTagNameNS(const namespaceURI,
                                          localName: wideString): TdomNodeList; virtual;
    function importNode(const importedNode: TdomNode;
                        const deep: boolean): TdomNode; virtual;
    function insertBefore(const newChild,
                                refChild: TdomNode): TdomNode; override;
    function prepareAttributes: boolean; virtual;
    function replaceChild(const newChild,
                                oldChild: TdomNode): TdomNode; override;
    function resolveEntityReferences(const opt: TdomEntityResolveOption): integer; override;
    function validate(const opt: TdomEntityResolveOption;
                      const intSubsetStartByteNumber,
                            intSubsetStartCharNumber,
                            intSubsetStartColumn,
                            intSubsetStartLine: Int64): boolean; virtual;

{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
    property ASModelsNS: TdomASModelCollectionNS read FASModelsNS; // xxx currently not used.
{$endif}
    property defaultView: TdomAbstractView read FDefaultView;
    property doctype: TdomDocumentType read FDoctype;
    property doctypeDecl: TdomDocumentTypeDecl read getDoctypeDecl;
    property documentElement: TdomElement read getDocumentElement;
    property documentUri: wideString read FDocumentUri write FDocumentUri;
    property domImplementation: TDomImplementation read FDomImpl;
    property inputEncoding: wideString read FInputEncoding write FInputEncoding;
    property modified: boolean read FModified write FModified;
    property xmlEncoding: wideString read FXmlEncoding write FXmlEncoding;
    property xmlStandalone: TdomStandalone read FXmlStandalone write FXmlStandalone;
    property xmlVersion: wideString read FXmlVersion write FXmlVersion;

    property OnAttrModified:          TdomAttrModifiedEvent read FOnAttrModified write FOnAttrModified;
    property OnCharacterDataModified: TdomNotifyNodeEvent read FOnCharacterDataModified write FOnCharacterDataModified;
    property OnNodeClearing:          TdomNotifyNodeEvent read FOnNodeClearing write FOnNodeClearing;
    property OnNodeInserted:          TdomNotifyNodeEvent read FOnNodeInserted write FOnNodeInserted;
    property OnNodeRemoving:          TdomNotifyNodeEvent read FOnNodeRemoving write FOnNodeRemoving;
  end;

  TXmlStandardDomReader = class;
  TdomASObject = class;

  TdomSeverity = (DOM_SEVERITY_WARNING,
                  DOM_SEVERITY_ERROR,
                  DOM_SEVERITY_FATAL_ERROR);

  IDomLocator = interface
    function GetEndByteNumber: Int64; stdcall;
    function GetEndCharNumber: Int64; stdcall;
    function GetEndColumnNumber: Int64; stdcall;
    function GetEndLineNumber: Int64; stdcall;
    function GetRelatedASObject: TdomASObject; stdcall;
    function GetRelatedNode: TdomNode; stdcall;
    function GetStartByteNumber: Int64; stdcall;
    function GetStartCharNumber: Int64; stdcall;
    function GetStartColumnNumber: Int64; stdcall;
    function GetStartLineNumber: Int64; stdcall;
    function GetUri: WideString; stdcall;    // xxx change to GetSystemId ?

    // xxx Remove properties?
    property EndByteNumber: Int64 read GetEndByteNumber;
    property EndCharNumber: Int64 read GetEndCharNumber;
    property EndColumnNumber: Int64 read GetEndColumnNumber;
    property EndLineNumber: Int64 read GetEndLineNumber;
    property RelatedASObject: TdomASObject read GetRelatedASObject;
    property RelatedNode: TdomNode read GetRelatedNode;
    property StartByteNumber: Int64 read GetStartByteNumber;
    property StartCharNumber: Int64 read GetStartCharNumber;
    property StartColumnNumber: Int64 read GetStartColumnNumber;
    property StartLineNumber: Int64 read GetStartLineNumber;
    property Uri: WideString read GetUri;     // xxx change to SystemId ?
  end;

  TdomError = class(TUtilsNoRefCount, IDomLocator)
  private
    FCode:              WideString;
    FEndByteNumber:     Int64;
    FEndCharNumber:     Int64;
    FEndColumnNumber:   Int64;
    FEndLineNumber:     Int64;
    FRelatedASObject:   TdomASObject;
    FRelatedException:  TXmlErrorType;
    FRelatedNode:       TdomNode;
    FStartByteNumber:   Int64;
    FStartCharNumber:   Int64;
    FStartColumnNumber: Int64;
    FStartLineNumber:   Int64;
    FUri:               WideString;
  protected
    function getSeverity: TdomSeverity; virtual;

    { IDomLocator interface methods: }
    function GetEndByteNumber: Int64; virtual; stdcall;
    function GetEndCharNumber: Int64; virtual; stdcall;
    function GetEndColumnNumber: Int64; virtual; stdcall;
    function GetEndLineNumber: Int64; virtual; stdcall;
    function GetRelatedASObject: TdomASObject; virtual; stdcall;
    function GetRelatedNode: TdomNode; virtual; stdcall;
    function GetStartByteNumber: Int64; virtual; stdcall;
    function GetStartCharNumber: Int64; virtual; stdcall;
    function GetStartColumnNumber: Int64; virtual; stdcall;
    function GetStartLineNumber: Int64; virtual; stdcall;
    function GetUri: WideString; virtual; stdcall;
  public
    constructor create(const ARelatedException: TXmlErrorType;
                       const AStartByteNumber,
                             AStartCharNumber,
                             AStartColumnNumber,
                             AStartLineNumber,
                             AEndByteNumber,
                             AEndCharNumber,
                             AEndColumnNumber,
                             AEndLineNumber: Int64;
                       const AUri: wideString;
                       const ARelatedASObject: TdomASObject;
                       const ARelatedNode: TdomNode;
                       const ACode: wideString); virtual;
    constructor createFromError(const AError: TdomError); virtual;
    constructor createFromLocator(const ARelatedException: TXmlErrorType;
                                  const ALocation: IDomLocator;
                                  const ACode: wideString); virtual;
    function cloneError: TdomError; virtual;

    property Code: WideString read FCode;
    property RelatedException: TXmlErrorType read FRelatedException;
    property Severity: TdomSeverity read getSeverity;

    { IDomLocator interface properties: }
    property EndByteNumber: Int64 read GetEndByteNumber;
    property EndCharNumber: Int64 read GetEndCharNumber;
    property EndColumnNumber: Int64 read GetEndColumnNumber;
    property EndLineNumber: Int64 read GetEndLineNumber;
    property RelatedASObject: TdomASObject read GetRelatedASObject;
    property RelatedNode: TdomNode read GetRelatedNode;
    property StartByteNumber: Int64 read GetStartByteNumber;
    property StartCharNumber: Int64 read GetStartCharNumber;
    property StartColumnNumber: Int64 read GetStartColumnNumber;
    property StartLineNumber: Int64 read GetStartLineNumber;
    property Uri: WideString read GetUri;
  end;

  TdomErrorClass = class of TdomError;

// Abstract Schema

  TdomASObject = class
  protected
    FName: wideString;
    FObjectType: TdomASObjectType;
    FOwnerModel: TdomASModel;
    function getName: wideString; virtual;
  public
    constructor create(const aOwner: TdomASModel;
                       const aName: wideString);
    property name: wideString read getName;
    property objectType: TdomASObjectType read FObjectType;
    property ownerModel: TdomASModel read FOwnerModel;
  end;

{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
  TdomCustomASObjectNS = class
  protected
    FIsNamespaceAware: boolean;
    FObjectType: TdomASObjectType;
    FOwnerModel: TdomCustomASModelNS;
    function getName: wideString; virtual; abstract;
  public
    constructor create(const aOwner: TdomCustomASModelNS);
    property isNamespaceAware: boolean read FIsNamespaceAware;
    property name: wideString read getName;
    property objectType: TdomASObjectType read FObjectType;
    property ownerModel: TdomCustomASModelNS read FOwnerModel;
  end;

  TdomASObjectNS = class(TdomCustomASObjectNS)
  protected
    FLocalName: wideString;
    FNamespaceURI: wideString;
    FPrefix: wideString;
    function getName: wideString; override;
  public
    constructor create(const aOwner: TdomASModelNS;
                       const aNamespaceURI,
                             aPrefix,
                             aLocalName: wideString);
    property localName: wideString read FLocalName;
    property namespaceURI: wideString read FNamespaceURI;
    property prefix: wideString read FPrefix;
  end;
{$endif}

  TdomASObjectList = class
  private
    FNodeList: TList;
  protected
    procedure clear;
    function appendASNode(const newNode: TdomASObject): TdomASObject; virtual;
    procedure Delete(const index: integer); virtual;
    function indexOf(const node: TdomASObject): integer; virtual;
    function insertBefore(const newNode,
                                refNode: TdomASObject): TdomASObject; virtual;
    function getLength: integer; virtual;
    function removeASNode(const oldNode: TdomASObject): TdomASObject; virtual;
  public
    constructor create;
    destructor destroy; override;
    function item(const index: integer): TdomASObject; virtual;
    property length: integer read getLength;
  end;

  TdomASNamedObjectMap = class
  protected
    FObjectList: TList;
    FOwnerObject: TdomASModel;
    function getLength: integer; virtual;
    function removeNamedItem(const name: wideString): TdomASObject; virtual;
    function setNamedItem(const arg: TdomASObject): TdomASObject; virtual;
    procedure clear; virtual;
  public
    constructor create(const aOwner: TdomASModel);
    destructor destroy; override;
    function getNamedItem(const name: wideString): TdomASObject; virtual;
    function item(const index: integer): TdomASObject; virtual;
    property length: integer read getLength;
    property ownerModel: TdomASModel read FOwnerObject;
  end;

{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
  TdomASObjectListNS = class
  private
    FNodeList: TList;
  protected
    procedure clear;
    function appendASNode(const newNode: TdomASObjectNS): TdomASObjectNS; virtual;
    procedure Delete(const index: integer); virtual;
    function indexOf(const node: TdomASObjectNS): integer; virtual;
    function insertBefore(const newNode,
                                refNode: TdomASObjectNS): TdomASObjectNS; virtual;
    function getLength: integer; virtual;
    function removeASNode(const oldNode: TdomASObjectNS): TdomASObjectNS; virtual;
  public
    constructor create;
    destructor destroy; override;
    function item(const index: integer): TdomASObjectNS; virtual;
    property length: integer read getLength;
  end;

  TdomASNamedObjectMapNS = class
  protected
    FObjectList: TList;
    FOwnerObject: TdomASModelNS;
    function getLength: integer; virtual;
    function removeNamedItem(const namespaceURI,
                                   localName: wideString): TdomASObjectNS; virtual;
    function setNamedItem(const arg: TdomASObjectNS): TdomASObjectNS; virtual;
    procedure clear; virtual;
  public
    constructor create(const aOwner: TdomASModelNS);
    destructor destroy; override;
    function getNamedItem(const namespaceURI,
                                localName: wideString): TdomASObjectNS; virtual;
    function item(const index: integer): TdomASObjectNS; virtual;
    property length: integer read getLength;
    property ownerModel: TdomASModelNS read FOwnerObject;
  end;
{$endif}

  TdomASModelCollection = class(TCustomOwnedObject)
  private
    FASModels: TList;
    function getDomImplementation: TDomImplementation;
    function getExternalSubset: TdomASModel;
    function getInternalSubset: TdomASModel;
    function getOwnerDocType: TdomDocumentTypeDecl;
  protected
    function createDefaultAttrList(const elementType: wideString;
                                     out listComplete: boolean): TUtilsNameValueList; virtual; // xxx move to public section?
    function findASAttributeDecl(const elementName,
                                       attributeName: wideString): TdomASAttributeDecl; virtual;
    function findASElementDecl(const name: wideString): TdomASElementDecl; virtual;
    function findASEntityDecl(const name: wideString): TdomASEntityDecl; virtual;
    function findASEntityReplacementText(const entityName: wideString;
                                           out replText: wideString): TXmlErrorType; virtual;
    function findASNotationDecl(const name: wideString): TdomASNotationDecl; virtual;
    function getItems(index: integer): TdomASModel; virtual;
    function getLength: integer; virtual;
    function normalizeAttributeDeclValue(const attrDecl: TdomASAttributeDecl;
                                           var error: TXmlErrorType): wideString; virtual;
    function refersToItself(const entDecl: TdomASEntityDecl;
                            const allowUnresolvableEntities: boolean): boolean; virtual;
    function refersToExternalEntity(const entityName: wideString): boolean; virtual;
    function refersToLTEntity(const entDecl: TdomASEntityDecl): boolean; virtual;
    function refersToUnparsedEntity(const entDecl: TdomASEntityDecl): boolean; virtual;
    function refersToXyz(const entDecl: TdomASEntityDecl;
                         const allowUnresolvableEntities: boolean;
                         const previousEntities: TUtilsWideStringList;
                         const whatToTest: integer): boolean; virtual;
    function sendErrorNotification(const xmlErrorType: TXmlErrorType;
                                   const relASObject: TdomASObject): boolean; virtual;

    property ASModels: TList read FASModels;
    property domImplementation: TDomImplementation read getDomImplementation;
  public
    constructor create(const aOwner: TdomDocumentTypeDecl);
    destructor destroy; override;
    procedure clearSubsets; virtual;
    function validate: boolean; virtual;

    property externalSubset: TdomASModel read getExternalSubset;
    property items[index: integer]: TdomASModel read getItems; default;
    property internalSubset: TdomASModel read getInternalSubset;
    property length: integer read getLength;
    property ownerDocType: TdomDocumentTypeDecl read getOwnerDocType;
  end;

{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
  TdomASModelCollectionNS = class
  protected
    FASModelsNS: TList;
    FOwnerDocument: TdomDocument;
    function findASAttributeDecl(const elementNamespaceURI,
                                       elementLocalName,
                                       attributeNamespaceURI,
                                       attributeLocalName: wideString): TdomASAttributeDeclNS;
    function findASElementDecl(const namespaceURI,
                                     localName: wideString): TdomASElementDeclNS; virtual;
    function findASEntityDecl(const namespaceURI,
                                    localName: wideString): TdomASEntityDeclNS; virtual;
    function findASNotationDecl(const namespaceURI,
                                      localName: wideString): TdomASNotationDeclNS; virtual;
    function getItems(index: integer): TdomASModelNS; virtual;
    function getLength: integer; virtual;
  public
    constructor create(const aOwner: TdomDocument);
    destructor destroy; override;
    function add(const model: TdomASModelNS): integer; virtual;
    procedure clear; virtual;
    procedure insert(const index: integer;
                           model: TdomASModelNS); virtual;
    function remove(const model: TdomASModelNS): integer; virtual;
    property items[index: integer]: TdomASModelNS read getItems; default;
    property length: integer read getLength;
    property ownerDocument: TdomDocument read FOwnerDocument;
  end;
{$endif}

  TdomASContentModel = class(TdomASObject)
  protected
    FAllowedChildTypes: set of TdomASContentModelType;
    FContentModelType: TdomASContentModelType;
    FFrequency: TdomASFrequency;
    FInuse: boolean;
    FOwnerElementDecl: TdomASElementDecl;
    FSubModels: TdomASObjectList;
    function validateChoiceNames(const source: TUtilsWideStringList;
                                   var index: integer;
                                       freq: TdomASFrequency;
                                   out isNonDeterministic: boolean): boolean; virtual;
    function validateElementNames(const source: TUtilsWideStringList;
                                    var index: integer;
                                        freq: TdomASFrequency;
                                    out isNonDeterministic: boolean): boolean; virtual;
    function validateNames2(const source: TUtilsWideStringList;
                              var index: integer;
                                  freq: TdomASFrequency;
                              out isNonDeterministic: boolean): boolean; virtual;
    function validateNames(const source: TUtilsWideStringList;
                             var index: integer;
                             out isNonDeterministic: boolean): boolean; virtual;
    function validateSequenceNames(const source: TUtilsWideStringList;
                                     var index: integer;
                                         freq: TdomASFrequency;
                                     out isNonDeterministic: boolean): boolean; virtual;
  public
    constructor create(const aOwnerElementDecl: TdomASElementDecl;
                       const aName: wideString;
                       const aContentModelType: TdomASContentModelType);
    destructor destroy; override;
    function appendSubModel(const newCM: TdomASContentModel): TdomASContentModel; virtual;
    function insertBeforeSubModel(const newCM,
                                        refCM: TdomASContentModel): TdomASContentModel; virtual;
    function removeSubModel(const oldCM: TdomASContentModel): TdomASContentModel; virtual;
    property contentModelType: TdomASContentModelType read FContentModelType;
    property frequency: TdomASFrequency read FFrequency write FFrequency default AS_REQUIRED_FRQ;
    property subModels: TdomASObjectList read FSubModels;
    property ownerElementDecl: TdomASElementDecl read FOwnerElementDecl;
  end;

{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
  TdomASContentModelNS = class(TdomASObjectNS)
  protected
    FAllowedChildTypes: set of TdomASContentModelType;
    FContentModelType: TdomASContentModelType;
    FInuse: boolean;
    FMaxOccurs: integer;
    FMinOccurs: integer;
    FOwnerElementDecl: TdomASElementDeclNS;
    FSubModels: TdomASObjectListNS;
    function getName: wideString; override;
    procedure setMaxOccurs(const value: integer); virtual;
    procedure setMinOccurs(const value: integer); virtual;
  public
    constructor create(const aOwnerElementDecl: TdomASElementDeclNS;
                       const aContentModelType: TdomASContentModelType);
    destructor destroy; override;
    function appendSubModel(const newCM: TdomASContentModelNS): TdomASContentModelNS; virtual;
    function insertBeforeSubModel(const newCM,
                                        refCM: TdomASContentModelNS): TdomASContentModelNS; virtual;
    function removeSubModel(const oldCM: TdomASContentModelNS): TdomASContentModelNS; virtual;
    property contentModelType: TdomASContentModelType read FContentModelType;
    property maxOccurs: integer read FMaxOccurs write setMaxOccurs default 1;
    property minOccurs: integer read FMinOccurs write setMinOccurs default 1;
    property subModels: TdomASObjectListNS read FSubModels;
    property ownerElementDecl: TdomASElementDeclNS read FOwnerElementDecl;
  end;
{$endif}

  TdomASAttributeDecl = class(TdomASObject)
  private
    FAttrType: TXmlDataType;
    FDefaultValue: wideString;
    FConstraintType: TdomAttrValueConstraint;
    FEnumeration: TUtilsWideStringList;
    FOwnerElementDecl: TdomASElementDecl;
  public
    constructor create(const aOwnerElementDecl: TdomASElementDecl;
                       const aAttrName,
                             aDefaultValue: wideString;
                       const aEnumeration: TUtilsWideStringList;
                       const aAttrType: TXmlDataType;
                       const aConstraintType: TdomAttrValueConstraint);
    destructor destroy; override;
    property attrType: TXmlDataType read FAttrType;
    property constraintType: TdomAttrValueConstraint read FConstraintType;
    property defaultValue: wideString read FDefaultValue;
    property enumeration: TUtilsWideStringList read FEnumeration;
    property ownerElementDecl: TdomASElementDecl read FOwnerElementDecl;
  end;

{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
  TdomASAttributeDeclNS = class(TdomASObjectNS)
  private
    FAttrType: TXmlDataType;
    FAttrValue: wideString;
    FConstraintType: TdomAttrValueConstraint;
    FEnumAttr: TUtilsWideStringList;
    FOwnerElementDecl: TdomASElementDeclNS;
  public
    constructor create(const aOwnerElementDecl: TdomASElementDeclNS;
                       const aNamespaceURI,
                             aPrefix,
                             aLocalName: wideString);
    destructor destroy; override;
    property attrType: TXmlDataType read FAttrType write FAttrType default AS_STRING_DATATYPE;
    property attrValue: wideString read FAttrValue write FAttrValue;
    property constraintType: TdomAttrValueConstraint read FConstraintType write FConstraintType default AVC_IMPLIED;
    property enumAttr: TUtilsWideStringList read FEnumAttr;
    property ownerElementDecl: TdomASElementDeclNS read FOwnerElementDecl;
  end;
{$endif}

  TdomASElementDecl = class(TdomASObject)
  protected
    FAllowedChildTypes: set of TdomASContentModelType;
    FAttributeDeclarations: TdomASNamedObjectMap;
    FContentModel: TdomASContentModel;
    FContentType: TdomASContentType;
    FCreatedContentModels: TdomASObjectList;
    procedure setContentType(const value: TdomASContentType); virtual;
  public
    constructor create(const aOwner: TdomASModel;
                       const aName: wideString;
                       const aContentType: TdomASContentType);
    destructor destroy; override;
    procedure clear; virtual;
    function createContentModel(const name: wideString;
                                const contentModelType: TdomASContentModelType): TdomASContentModel; virtual;
    function findASAttributeDecl(const name: wideString): TdomASAttributeDecl; virtual;
    procedure freeAndNilContentModel(var cm: TdomASContentModel); virtual;
    function removeASAttributeDecl(const name: wideString): boolean; virtual;
    function replaceContentModel(const newContentModel: TdomASContentModel): TdomASContentModel; virtual;
    function setASAttributeDecl(const aAttrName,
                                      aAttrValue: wideString;
                                const aEnumeration: TUtilsWideStringList;
                                const aAttrType: TXmlDataType;
                                const aConstraintType: TdomAttrValueConstraint;
                                  out attributeDecl: TdomASAttributeDecl): boolean; virtual;

    property attributeDecls: TdomASNamedObjectMap read FAttributeDeclarations;
    property contentModel: TdomASContentModel read FContentModel;
    property contentType: TdomASContentType read FContentType write setContentType;
    property createdContentModels: TdomASObjectList read FCreatedContentModels;
  end;

{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
  TdomASElementDeclNS = class(TdomASObjectNS)
  protected
    FAllowedChildTypes: set of TdomASContentModelType;
    FAttributeDeclarations: TdomASNamedObjectMapNS;
    FContentModel: TdomASContentModelNS;
    FContentType: TdomASContentType;
    FCreatedContentModels: TdomASObjectListNS;
  public
    constructor create(const aOwner: TdomASModelNS;
                       const aNamespaceURI,
                             aPrefix,
                             aLocalName: wideString;
                       const aContentType: TdomASContentType);
    destructor destroy; override;
    procedure clear; virtual;
    function createContentModel(const contentModelType: TdomASContentModelType): TdomASContentModelNS; virtual;
    function findASAttributeDecl(const namespaceURI,
                                       localName: wideString): TdomASAttributeDeclNS; virtual;
    procedure freeAndNilContentModel(var cm: TdomASContentModelNS); virtual;
    function removeASAttributeDecl(const namespaceURI,
                                         localName: wideString): boolean; virtual;
    function replaceContentModel(const newContentModel: TdomASContentModelNS): TdomASContentModelNS; virtual;
    function setASAttributeDecl(const namespaceURI,
                                      prefix,
                                      localName: wideString;
                                  out attributeDecl: TdomASAttributeDeclNS): boolean; virtual;
    property attributeDecls: TdomASNamedObjectMapNS read FAttributeDeclarations;
    property contentModel: TdomASContentModelNS read FContentModel;
    property contentType: TdomASContentType read FContentType;
    property createdContentModels: TdomASObjectListNS read FCreatedContentModels;
  end;
{$endif}

  TdomASEntityDecl = class(TdomASObject)
  private
    FContainsLT: TdomTrinarean;
    FEntityRefs: TUtilsWideStringList;
    FNotationName: wideString;
    FPublicId: wideString;
    FReplacementText: wideString;
    FSystemId: wideString;
    FUsability: TdomASEntityUsability;
    function getEntityType: TdomASEntityType;
    procedure setReplacementText(const S: wideString);
  protected
    function getIsParsedEntity: boolean; virtual;
  public
    constructor create(const aOwner: TdomASModel;
                       const aName,
                             aReplacementText,
                             aPublicId,
                             aSystemId,
                             aNotationName: wideString);
    destructor destroy; override;
    function resolve: boolean; virtual;

    property containsLT: TdomTrinarean read FContainsLT;
    property entityRefs: TUtilsWideStringList read FEntityRefs;
    property entityType: TdomASEntityType read getEntityType;
    property isParsedEntity: boolean read getIsParsedEntity;
    property notationName: wideString read FNotationName;
    property publicId: wideString read FPublicId;
    property replacementText: wideString read FReplacementText;
    property systemId: wideString read FSystemId;
    property usability: TdomASEntityUsability read FUsability;
  end;

{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
  TdomASEntityDeclNS = class(TdomASObjectNS)
  private
    function establishUsability(const S: wideString): TdomASEntityUsability;
    function getEntityType: TdomASEntityType;
    procedure setReplacementText(const S: wideString);
  protected
    FNotationName: wideString;
    FPublicId: wideString;
    FReplacementText: wideString;
    FSystemId: wideString;
    FUsability: TdomASEntityUsability;
    function getIsParsedEntity: boolean; virtual;
  public
    constructor create(const aOwner: TdomASModelNS;
                       const aNamespaceURI,
                             aPrefix,
                             aLocalName,
                             aReplacementText,
                             aPublicId,
                             aSystemId,
                             aNotationName: wideString);
    property entityType: TdomASEntityType read getEntityType;
    property isParsedEntity: boolean read getIsParsedEntity;
    property notationName: wideString read FNotationName;
    property publicId: wideString read FPublicId;
    property systemId: wideString read FSystemId;
    property usability: TdomASEntityUsability read FUsability;
  end;
{$endif}

  TdomASNotationDecl = class(TdomASObject)
  protected
    FPublicId: wideString;
    FSystemId: wideString;
  public
    constructor create(const aOwner: TdomASModel;
                       const aName,
                             aPublicId,
                             aSystemId: wideString);
    property publicId: wideString read FPublicId;
    property systemId: wideString read FSystemId;
  end;

{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
  TdomASNotationDeclNS = class(TdomASObjectNS)
  protected
    FPublicId: wideString;
    FSystemId: wideString;
  public
    constructor create(const aOwner: TdomASModelNS;
                       const aNamespaceURI,
                             aPrefix,
                             aLocalName,
                             aPublicId,
                             aSystemId: wideString);
    property publicId: wideString read FPublicId;
    property systemId: wideString read FSystemId;
  end;
{$endif}

  TdomASModel = class(TCustomOwnedObject)
  private
    FLocation: wideString;
    function getOwnerCollection: TdomASModelCollection;
    function getDomImplementation: TdomImplementation;
  protected
    FElementDeclarations: TdomASNamedObjectMap;
    FEntityDeclarations: TdomASNamedObjectMap;
    FHint: wideString;
    FNotationDeclarations: TdomASNamedObjectMap;
  public
    constructor create(const aOwner: TdomASModelCollection);
    destructor destroy; override;
    procedure clear; override;
    function findASAttributeDecl(const elementName,
                                       attributeName: wideString): TdomASAttributeDecl; virtual;
    function findASElementDecl(const name: wideString): TdomASElementDecl; virtual;
    function findASEntityDecl(const name: wideString): TdomASEntityDecl; virtual;
    function findASNotationDecl(const name: wideString): TdomASNotationDecl; virtual;
    function removeASElementDecl(const name: wideString): boolean; virtual;
    function removeASEntityDecl(const name: wideString): boolean; virtual;
    function removeASNotationDecl(const name: wideString): boolean; virtual;
    function setASElementDecl(const name: wideString;
                              const contentType: TdomASContentType;
                                out elementDecl: TdomASElementDecl): boolean; virtual;
    function setASEntityDecl(const name,
                                   replacementText,
                                   publicId,
                                   systemId,
                                   notationName: wideString;
                               out entityDecl: TdomASEntityDecl): boolean; virtual;
    function setASNotationDecl(const name,
                                     publicId,
                                     systemId: wideString;
                                 out notationDecl: TdomASNotationDecl): boolean; virtual;

    property domImplementation: TdomImplementation read getDomImplementation;
    property elementDecls: TdomASNamedObjectMap read FElementDeclarations;
    property entityDecls: TdomASNamedObjectMap read FEntityDeclarations;
    property hint: wideString read FHint write FHint;
    property location: wideString read FLocation write FLocation;  // xxx rename to 'systemId'?
    property notationDecls: TdomASNamedObjectMap read FNotationDeclarations;
    property ownerCollection: TdomASModelCollection read getOwnerCollection;
  end;

{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
  TdomCustomASModelNS = class
  protected
    FDomImpl: TDomImplementation;
    FHint: wideString;
    FIsNamespaceAware: boolean;
    FLocation: wideString;
    FOwnerCollections: TList;
  public
    constructor create(const aOwner: TDomImplementation);
    destructor destroy; override;
    procedure clear; virtual; abstract;
    property domImplementation: TDomImplementation read FDomImpl;
    property hint: wideString read FHint write FHint;
    property isNamespaceAware: boolean read FIsNamespaceAware;
    property location: wideString read FLocation write FLocation;
    property ownerCollections: TList read FOwnerCollections;
  end;

  TdomASModelNS = class(TdomCustomASModelNS)
  protected
    FElementDeclarations: TdomASNamedObjectMapNS;
    FEntityDeclarations: TdomASNamedObjectMapNS;
    FNotationDeclarations: TdomASNamedObjectMapNS;
  public
    constructor create(const aOwner: TDomImplementation);
    destructor destroy; override;
    procedure clear; override;
    function findASAttributeDecl(const elementNamespaceURI,
                                       elementLocalName,
                                       attributeNamespaceURI,
                                       attributeLocalName: wideString): TdomASAttributeDeclNS;
    function findASElementDecl(const aNamespaceURI,
                                     aLocalName: wideString): TdomASElementDeclNS; virtual;
    function findASEntityDecl(const aNamespaceURI,
                                    aLocalName: wideString): TdomASEntityDeclNS; virtual;
    function findASNotationDecl(const aNamespaceURI,
                                      aLocalName: wideString): TdomASNotationDeclNS; virtual;
    function removeASElementDecl(const aNamespaceURI,
                                       aLocalName: wideString): boolean; virtual;
    function removeASEntityDecl(const aNamespaceURI,
                                      aLocalName: wideString): boolean; virtual;
    function removeASNotationDecl(const aNamespaceURI,
                                        aLocalName: wideString): boolean; virtual;
    function setASElementDecl(const aNamespaceURI,
                                    aPrefix,
                                    aLocalName: wideString;
                              const contentType: TdomASContentType;
                                out elementDecl: TdomASElementDeclNS): boolean; virtual;
    function setASEntityDecl(const aNamespaceURI,
                                   aPrefix,
                                   aLocalName,
                                   aReplacementText,
                                   aPublicId,
                                   aSystemId,
                                   aNotationName: wideString;
                               out entityDecl: TdomASEntityDeclNS): boolean; virtual;
    function setASNotationDecl(const aNamespaceURI,
                                     aPrefix,
                                     aLocalName,
                                     publicId,
                                     systemId: wideString;
                                 out notationDecl: TdomASNotationDeclNS): boolean; virtual;
    property elementDecls: TdomASNamedObjectMapNS read FElementDeclarations;
    property entityDecls: TdomASNamedObjectMapNS read FEntityDeclarations;
    property notationDecls: TdomASNamedObjectMapNS read FNotationDeclarations;
  end;
{$endif}


// Views

  TdomAbstractView = class
  protected
    FDocument: TdomDocument;
  public
    property document: TdomDocument read FDocument;
  end;

  TdomStyleSheet = class
  private
    function getStyleSheetType: wideString; virtual; abstract;
    function getDisabled: boolean; virtual; abstract;
    procedure setDisabled(const value: boolean); virtual; abstract;
    function getOwnerNode: TdomNode; virtual; abstract;
    function getParentStyleSheet: TdomStyleSheet; virtual; abstract;
    function getHref: wideString; virtual; abstract;
    function getTitle: wideString; virtual; abstract;
    function getMedia: TdomMediaList; virtual; abstract;
  public
    property styleSheetType: wideString read getStyleSheetType;
    property disabled: boolean read getDisabled write setDisabled;
    property ownerNode: TdomNode read getOwnerNode;
    property parentStyleSheet: TdomStyleSheet read getParentStyleSheet;
    property href: wideString read getHref;
    property title: wideString read getTitle;
    property media: TdomMediaList read getMedia;
  end;

  TdomMediaList = class
  private
    function getCssText: wideString; virtual; abstract;
    procedure setCssText(const value: wideString); virtual; abstract;
    function getLength: integer; virtual; abstract;
  public
    function item(const index: integer): TdomStyleSheet; virtual; abstract;
    procedure Delete(const oldMedium: wideString); virtual; abstract;
    procedure append(const newMedium: wideString); virtual; abstract;
    property length: integer read getLength;
    property cssText: wideString read getCssText write setCssText;
  end;

  TdomStyleSheetList = class
  private
    function getLength: integer; virtual; abstract;
  public
    function item(const index: integer): TdomStyleSheet; virtual; abstract;
    property length: integer read getLength;
  end;

  TdomDocumentStyle = class
  private
    function getStyleSheets: TdomStyleSheetList; virtual; abstract;
  public
    property styleSheets: TdomStyleSheetList read getStyleSheets;
  end;

  TXmlSourceCode = class (TList)
  private
    procedure calculatePieceOffset(const startItem: integer);
    function  getNameOfFirstTag: wideString;
    function  getText: wideString;
  public
    function  add(Item: Pointer): integer;
    procedure clear; override;
    procedure clearAndFree; virtual;
    procedure Delete(index: integer);
    procedure exchange(index1, index2: integer);
    function  getPieceAtPos(pos: integer): TXmlSourceCodePiece;
    procedure insert(index: integer; item: pointer);
    procedure move(curIndex, newIndex: integer);
    procedure pack;
    function  remove(Item: Pointer): integer;
    procedure sort(Compare: TListSortCompare);
    property  nameOfFirstTag: wideString read getNameOfFirstTag;
    property  text: wideString read getText;
  end;

  TXmlSourceCodePiece = class
  private
    FPieceType: TdomPieceType;
    FText: wideString;
    FOffset: integer;
    FOwner: TXmlSourceCode;
  public
    constructor create(const pt: TdomPieceType); virtual;
    property pieceType: TdomPieceType read FPieceType;
    property text: wideString read FText write FText;
    property offset: integer read FOffset;
    property ownerSourceCode: TXmlSourceCode read FOwner;
  end;


// Parser

  TCustomResourceResolver = class(TDomBaseComponent)
  public
    function resolveResource(const aBaseURI: wideString;
                               var publicId,
                                   systemId: wideString): TStream; virtual; abstract;
  end;

  TStandardResourceResolver = class(TCustomResourceResolver)
  private
    FOnResolveResource: TdomResolveResourceEvent;
  protected
    function acquireStreamFromUri(const URI: wideString): TStream; virtual;
  public
    function resolveResource(const aBaseURI: wideString;
                               var publicId,
                                   systemId: wideString): TStream; override;
  published
    property OnResolveResource: TdomResolveResourceEvent read FOnResolveResource write FOnResolveResource;
  end;

  TdomXMLDeclType = ( DT_UNKNOWN,
                      DT_XML_DECLARATION,
                      DT_TEXT_DECLARATION,
                      DT_XML_OR_TEXT_DECLARATION,
                      DT_UNSPECIFIED );

  TXmlInputSource = class(TUtilsUCS4Reader)   
  private
    FDeclType: TdomXMLDeclType;
    FHasMalformedDecl: Boolean;
    FInputEncoding: WideString;
    FInvalidEncoding: Boolean;
    FPublicId: WideString;
    FSystemId: WideString;
    FXmlEncoding: WideString;
    FXmlStandalone: TdomStandalone;
    FXmlVersion: WideString;
    function evaluateXmlOrTextDecl(out declType: TdomXMLDeclType;
                                   out version,
                                       encName: wideString;
                                   out standalone: TdomStandalone;
                                   out invalidEnc: boolean): boolean;
  protected
    function GetCurrentCodePoint: UCS4Char; virtual;
    function GetNextCodePoint: UCS4Char; virtual;
    function GetPreviousCodePoint: UCS4Char; virtual;
  public
    constructor create(const stream: TStream;
                       const aPublicId,
                             aSystemId: wideString;
                       const aBufSize: integer;
                       const defaultEncoding: wideString;
                       const inclDecl: boolean;
                       const initialByteCount,
                             initialCharCount,
                             initialRegularCharsInLine,
                             initialTabsInLine,
                             initialLine: Int64);

    property bufSize;
    property currentCodePoint: UCS4Char  read GetCurrentCodePoint;
    property declType: TdomXMLDeclType read FDeclType;
    property hasMalformedDecl: boolean read FHasMalformedDecl;
    property inputEncoding: wideString read FInputEncoding;
    property invalidEncoding: boolean read FInvalidEncoding;
    property nextCodePoint: UCS4Char  read GetNextCodePoint;
    property previousCodePoint: UCS4Char read GetPreviousCodePoint;
    property publicId: wideString read FPublicId;
    property systemId: wideString read FSystemId;
    property xmlEncoding: wideString read FXmlEncoding;
    property xmlStandalone: TdomStandalone read FXmlStandalone;
    property xmlVersion: wideString read FXmlVersion;
  end;

  TXmlDocTokenType = (
    XML_CDATA_TOKEN,
    XML_CHAR_REF_DEC_TOKEN,
    XML_CHAR_REF_HEX_TOKEN,
    XML_COMMENT_TOKEN,
    XML_DOCTYPE_TOKEN,
    XML_EMPTY_ELEMENT_TAG_TOKEN,
    XML_END_OF_SOURCE_TOKEN,
    XML_END_TAG_TOKEN,
    XML_ENTITY_REF_TOKEN,
    XML_PCDATA_TOKEN,
    XML_PI_TOKEN,
    XML_START_OF_SOURCE_TOKEN,
    XML_START_TAG_TOKEN
  );

  TXmlCustomTokenizer = class(TUtilsNoRefCount, IDomLocator)
  private
    FTabWidth: integer;
  protected
    FClue: wideString;
    FErrorType: TXmlErrorType;
    FInputSource: TXmlInputSource;
    FTokenEnd: TUtilsUCS4CharData;
    FTokenStart: TUtilsUCS4CharData;
    FTokenValue: TUtilsCustomWideStr;
    function getTokenValue: wideString; virtual;

    { IDomLocator interface methods: }
    function GetEndByteNumber: Int64; virtual; stdcall;
    function GetEndCharNumber: Int64; virtual; stdcall;
    function GetEndColumnNumber: Int64; virtual; stdcall;
    function GetEndLineNumber: Int64; virtual; stdcall;
    function GetRelatedASObject: TdomASObject; virtual; stdcall;
    function GetRelatedNode: TdomNode; virtual; stdcall;
    function GetStartByteNumber: Int64; virtual; stdcall;
    function GetStartCharNumber: Int64; virtual; stdcall;
    function GetStartColumnNumber: Int64; virtual; stdcall;
    function GetStartLineNumber: Int64; virtual; stdcall;
    function GetUri: WideString; virtual; stdcall;
  public
    constructor create(const aInputSource: TXmlInputSource;
                       const aTabWidth: cardinal);
    destructor destroy; override;
    procedure next; virtual; abstract;

    property clue: wideString read FClue;
    property errorType: TXmlErrorType read FErrorType;
    property tabWidth : integer read FTabWidth;
    property tokenValue: wideString read getTokenValue;
  end;

  TXmlDocTokenizer = class(TXmlCustomTokenizer)
  protected
    FTokenType: TXmlDocTokenType;
  public
    constructor create(const aInputSource: TXmlInputSource;
                       const aTabWidth: cardinal);
    procedure next; override;

    property tokenType: TXmlDocTokenType read FTokenType;
  end;

  TXmlDoctypeDeclTokenType = (
    DOCTYPE_END_OF_SOURCE_TOKEN,
    DOCTYPE_INTSUBSET_TOKEN,
    DOCTYPE_NAME_TOKEN,
    DOCTYPE_PUBID_TOKEN,
    DOCTYPE_START_OF_SOURCE_TOKEN,
    DOCTYPE_SYSID_TOKEN
  );

  TXmlDoctypeDeclTokenizer = class(TXmlCustomTokenizer)
  protected
    FTokenType: TXmlDoctypeDeclTokenType;
  public
    constructor create(const aInputSource: TXmlInputSource;
                       const aTabWidth: cardinal);
    procedure next; override;

    property tokenType: TXmlDoctypeDeclTokenType read FTokenType;
  end;

  TXmlDtdTokenType = (
    DTD_ATTLIST_DECL_TOKEN,
    DTD_COMMENT_TOKEN,
    DTD_ELEMENT_DECL_TOKEN,
    DTD_END_OF_CONDITIONAL_SECTION_TOKEN,
    DTD_END_OF_SOURCE_TOKEN,
    DTD_ENTITY_DECL_TOKEN,
    DTD_IGNORABLE_WHITESPACE_TOKEN,
    DTD_INVALID_MARKUP_TOKEN,
    DTD_NOTATION_DECL_TOKEN,
    DTD_PARAMETER_ENTITY_REF_TOKEN,
    DTD_PI_TOKEN,
    DTD_START_OF_CONDITIONAL_SECTION_TOKEN,
    DTD_START_OF_SOURCE_TOKEN
  );

  TXmlDtdAbstractTokenType = (
    DTD_ABSTRACT_ATTLIST_DECL_TOKEN,
    DTD_ABSTRACT_COMMENT_TOKEN,
    DTD_ABSTRACT_ELEMENT_DECL_TOKEN,
    DTD_ABSTRACT_END_OF_CONDITIONAL_SECTION_TOKEN,
    DTD_ABSTRACT_END_OF_SOURCE_TOKEN,
    DTD_ABSTRACT_ENTITY_DECL_TOKEN,
    DTD_ABSTRACT_IGNORABLE_WHITESPACE_TOKEN,
    DTD_ABSTRACT_INVALID_MARKUP_TOKEN,
    DTD_ABSTRACT_NOTATION_DECL_TOKEN,
    DTD_ABSTRACT_PARAMETER_ENTITY_DECL_TOKEN,
    DTD_ABSTRACT_PARAMETER_ENTITY_REF_TOKEN,
    DTD_ABSTRACT_PI_TOKEN,
    DTD_ABSTRACT_START_OF_CONDITIONAL_SECTION_TOKEN,
    DTD_ABSTRACT_START_OF_SOURCE_TOKEN
  );

  TXmlDtdTokenizer = class(TXmlCustomTokenizer)
  protected
    FTokenType: TXmlDtdTokenType;
  public
    constructor create(const aInputSource: TXmlInputSource;
                       const aTabWidth: cardinal;
                       const XmlDeclarationAllowed: boolean);
    procedure next; override;
    procedure nextEndOfIgnoredCondSect; virtual;

    property tokenType: TXmlDtdTokenType read FTokenType;
  end;

  TdomPERepository = class;
  TXmlExtSubsetTokenizer = class;

  TXmlCustomSubsetTokenizer = class(TUtilsNoRefCount, IDomLocator)
  protected
    FClue: wideString;
    FErrorType: TXmlErrorType;
    FLocator: IDomLocator;
    FPEInputSource: TXmlInputSource;
    FPERepository: TdomPERepository;
    FPEStrStream: TUtilsWideStringStream;
    FPETokenizer: TXmlExtSubsetTokenizer;
    FTokenData: wideString;
    FTokenName: wideString;
    FTokenType: TXmlDtdAbstractTokenType;
    FXmlDtdTokenizer: TXmlDtdTokenizer;
    function addParameterEntity(const PEName,
                                      PEData,
                                      baseUri: wideString): TXmlErrorType;
    procedure finalizePETokenizer;
    function getClue: wideString; virtual;
    function getErrorType: TXmlErrorType; virtual;
    function getSystemId: wideString; virtual;
    function getTabWidth: integer; virtual;
    function getTokenData: wideString; virtual;
    function getTokenName: wideString; virtual;
    function getTokenType: TXmlDtdAbstractTokenType ; virtual;
    function getXmlDeclarationAllowed: Boolean; virtual; abstract;
    function initializePETokenizer(const PEName: wideString;
                                   const aTabWidth,
                                         bufSize: integer): TXmlErrorType; virtual;
    procedure splitNameFromData(const S: wideString;
                                  out name,
                                      data: wideString);
    function translate(const token: TXmlDtdTokenType): TXmlDtdAbstractTokenType;

    { IDomLocator interface methods: }
    function GetEndByteNumber: Int64; virtual; stdcall;
    function GetEndCharNumber: Int64; virtual; stdcall;
    function GetEndColumnNumber: Int64; virtual; stdcall;
    function GetEndLineNumber: Int64; virtual; stdcall;
    function GetRelatedASObject: TdomASObject; virtual; stdcall;
    function GetRelatedNode: TdomNode; virtual; stdcall;
    function GetStartByteNumber: Int64; virtual; stdcall;
    function GetStartCharNumber: Int64; virtual; stdcall;
    function GetStartColumnNumber: Int64; virtual; stdcall;
    function GetStartLineNumber: Int64; virtual; stdcall;
    function GetUri: WideString; virtual; stdcall;
  public
    constructor create(const aInputSource: TXmlInputSource;
                       const aTabWidth: cardinal;
                       const aPERepository: TdomPERepository);
    destructor destroy; override;
    procedure next; virtual; abstract;

    property clue: wideString read getClue;
    property errorType: TXmlErrorType read getErrorType;
    property PERepository: TdomPERepository read FPERepository;
    property systemId: wideString read getSystemId;
    property tabWidth: integer read getTabWidth;
    property tokenData: wideString read getTokenData;
    property tokenName: wideString read getTokenName;
    property tokenType: TXmlDtdAbstractTokenType read getTokenType;
  end;

  TXmlExtSubsetTokenizer = class(TXmlCustomSubsetTokenizer)
  protected
    FCondSectCount: Integer;
    function includePERefs(const S: wideString;
                             var errType: TXmlErrorType): wideString;
    function includePERefsInAttrDecl(const S: wideString;
                                         var errType: TXmlErrorType): wideString;
    function includePERefsInEntityDecl(const S: wideString;
                                         var errType: TXmlErrorType): wideString;
    function getXmlDeclarationAllowed: Boolean; override;
  public
    constructor create(const aInputSource: TXmlInputSource;
                       const aTabWidth: cardinal;
                       const aPERepository: TdomPERepository);
    procedure next; override;
  end;

  TXmlIntSubsetTokenizer = class(TXmlCustomSubsetTokenizer)
  private
    FResolvePEs: boolean;
  protected
    procedure setResolvePEs(const value: boolean); virtual;
    function getXmlDeclarationAllowed: Boolean; override;
  public
    constructor create(const aInputSource: TXmlInputSource;
                       const aTabWidth: cardinal;
                       const aPERepository: TdomPERepository);
    procedure next; override;
    property resolvePEs: boolean read FResolvePEs write setResolvePEs default true;
  end;

  TXmlOutputSource = class(TUtilsCustomWriter)
  private
    FCodec: TCustomUnicodeCodec;
  protected
    function getCodecClass: TUnicodeCodecClass; virtual;
    function getWriteLFOption: TCodecWriteLFOption; virtual;
    procedure setCodecClass(const value: TUnicodeCodecClass); virtual;
    procedure setWriteLFOption(const value: TCodecWriteLFOption); virtual;
    procedure WriteEventHandler(      Sender: TObject;
                                const Buf;
                                      Count: Longint); virtual;
  public
    constructor create(const stream: TStream;
                       const bufSize: integer);
    destructor destroy; override;
    procedure writeUCS4Char(const C: UCS4Char;
                              out byteCount: integer); virtual;

    property bufSize;
    property codecClass: TUnicodeCodecClass read getCodecClass write setCodecClass;  // The default is TUTF8Codec
    property writeLFOption: TCodecWriteLFOption read getWriteLFOption write setWriteLFOption default lwCRLF;
  end;

  TXmlCustomParser = class;
  TXmlToDomParser = class;
  TXmlCustomReader = class;
  TXmlStandardDocReader = class;
  TXmlStandardDtdReader = class;
  TXmlSignal = class;
  TXmlCustomHandler = class;

  TXmlProcessingEvent = procedure(sender: TObject;
                                  signal: TXmlSignal;
                                  var accept: boolean) of object;

  TXmlPostProcessingEvent = procedure(sender: TObject;
                                      signal: TXmlSignal) of object;

  TdomPERepository = class
  private
    FOwner: TXmlCustomReader;
    FPEMap: TdomOwnerNamedNodeMap;
    function getDomImplementation: TDomImplementation;
  public
    constructor create(const aOwner: TXmlCustomReader);
    destructor destroy; override;
    function add(const name,
                       value: wideString): boolean; overload; virtual;
    function add(const name,
                       baseUri,
                       pubId,
                       sysId: wideString): boolean; overload; virtual;
    procedure clear; virtual;
    function resolvePE(const name: wideString;
                         out value,
                             pubId,
                             sysId: wideString): TXmlErrorType; virtual;
    property domImplementation: TDomImplementation read getDomImplementation;
    property ownerReader: TXmlCustomReader read FOwner;
  end;

  TdomPEInfoObject = class(TdomCustomNode)
  private
    FBaseUri: wideString;
    FRepository: TdomPERepository;
    FEntityType: TdomEntityType;
    FLiteralValue: wideString;
    FNodeName: wideString;
    FPublicId: wideString;
    FSystemId: wideString;
  protected
    function getLiteralValue: wideString; virtual;
    function getNodeName: wideString; override;
  public
    constructor create(const aOwner: TdomPERepository;
                       const entityName,
                             litValue: wideString);
    constructor createExtParsed(const aOwner: TdomPERepository;
                                const entityName,
                                      aBaseUri,
                                      pubId,
                                      sysId: wideString);
    function entityURI: wideString;

    property baseUri: wideString read FBaseUri;
    property entityType: TdomEntityType read FEntityType;
    property literalValue: wideString read getLiteralValue;
    property publicId: wideString read FPublicId;
    property repository: TdomPERepository read FRepository;
    property systemId: wideString read FSystemId;
  end;

{ XML Fragments }

  TXmlSignalScope = set of ( ssDoc, ssDtd );

  TXmlSignal = class(TUtilsNoRefCount, IDomLocator)
  private
    FEndByteNumber:     Int64;
    FEndCharNumber:     Int64;
    FEndColumnNumber:   Int64;
    FEndLineNumber:     Int64;
    FReader:            TXmlCustomReader;
    FRelatedASObject:   TdomASObject;
    FRelatedNode:       TdomNode;
    FStartByteNumber:   Int64;
    FStartCharNumber:   Int64;
    FStartColumnNumber: Int64;
    FStartLineNumber:   Int64;
    FUri:               WideString;
  protected
    { IDomLocator interface methods: }
    function GetEndByteNumber: Int64; virtual; stdcall;
    function GetEndCharNumber: Int64; virtual; stdcall;
    function GetEndColumnNumber: Int64; virtual; stdcall;
    function GetEndLineNumber: Int64; virtual; stdcall;
    function GetRelatedASObject: TdomASObject; virtual; stdcall;
    function GetRelatedNode: TdomNode; virtual; stdcall;
    function GetStartByteNumber: Int64; virtual; stdcall;
    function GetStartCharNumber: Int64; virtual; stdcall;
    function GetStartColumnNumber: Int64; virtual; stdcall;
    function GetStartLineNumber: Int64; virtual; stdcall;
    function GetUri: WideString; virtual; stdcall;
  public
    constructor Create(const AReader: TXmlCustomReader;
                       const AStartByteNumber,
                             AStartCharNumber,
                             AStartColumnNumber,
                             AStartLineNumber,
                             AEndByteNumber,
                             AEndCharNumber,
                             AEndColumnNumber,
                             AEndLineNumber: Int64;
                       const AUri: wideString;
                       const ARelatedASObject: TdomASObject;
                       const ARelatedNode: TdomNode); virtual;
    constructor CreateFromLocator(const AReader: TXmlCustomReader;
                                  const Location: IDomLocator); virtual;
    procedure CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                  out Flaw: WideString); virtual;
    function CloneSignal(const AReader: TXmlCustomReader): TXmlSignal; virtual;
    function Scope: TXmlSignalScope; virtual; abstract;

    property Reader: TXmlCustomReader read FReader;

    { IDomLocator interface properties: }      // xxx Revisit
    property EndByteNumber: Int64 read GetEndByteNumber;
    property EndCharNumber: Int64 read GetEndCharNumber;
    property EndColumnNumber: Int64 read GetEndColumnNumber;
    property EndLineNumber: Int64 read GetEndLineNumber;
    property RelatedASObject: TdomASObject read GetRelatedASObject;
    property RelatedNode: TdomNode read GetRelatedNode;
    property StartByteNumber: Int64 read GetStartByteNumber;
    property StartCharNumber: Int64 read GetStartCharNumber;
    property StartColumnNumber: Int64 read GetStartColumnNumber;
    property StartLineNumber: Int64 read GetStartLineNumber;
    property Uri: WideString read GetUri;
  end;

  TXmlSignalClass = class of TXmlSignal;

  { Special XML Signals }

  TXmlCompletedSignal = class(TXmlSignal)
  public
    function Scope: TXmlSignalScope; override;
  end;

  TXmlAbortedSignal = class(TXmlSignal)
  public
    function Scope: TXmlSignalScope; override;
  end;

  { Non-DTD XML Signals }

  TXmlCDATASignal = class(TXmlSignal)
  private
    FData: WideString;
  public
    procedure CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                  out Flaw: WideString); override;
    function CloneSignal(const AReader: TXmlCustomReader): TXmlSignal; override;
    function Scope: TXmlSignalScope; override;

    property Data: WideString read FData write FData;
  end;

  TXmlDoctypeSignal = class(TXmlSignal)
  private
    FData: WideString;
    FDoctypeName: WideString;
    FIntSubsetByteNumber: Int64;
    FIntSubsetCharNumber: Int64;
    FIntSubsetStartColumn: Int64;
    FIntSubsetStartLine: Int64;
    FPublicId: WideString;
    FSystemId: WideString;
  public
    procedure CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                  out Flaw: WideString); override;
    function CloneSignal(const AReader: TXmlCustomReader): TXmlSignal; override;
    function Scope: TXmlSignalScope; override;

    property Data: WideString read FData write FData;
    property DoctypeName: WideString read FDoctypeName write FDoctypeName;
    property IntSubsetStartByteNumber: Int64 read FIntSubsetByteNumber write FIntSubsetByteNumber;
    property IntSubsetStartCharNumber: Int64 read FIntSubsetCharNumber write FIntSubsetCharNumber;
    property IntSubsetStartColumn: Int64 read FIntSubsetStartColumn write FIntSubsetStartColumn;
    property IntSubsetStartLine: Int64 read FIntSubsetStartLine write FIntSubsetStartLine;
    property PublicId: WideString read FPublicId write FPublicId;
    property SystemId: WideString read FSystemId write FSystemId;
  end;

  TXmlEndElementSignal = class(TXmlSignal)
  private
    FTagName: WideString;
  public
    procedure CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                  out Flaw: WideString); override;
    function CloneSignal(const AReader: TXmlCustomReader): TXmlSignal; override;
    function Scope: TXmlSignalScope; override;

    property TagName: WideString read FTagName write FTagName;
  end;

  TXmlEndPrefixMappingSignal = class(TXmlSignal)
  private
    FPrefix: WideString;
  public
    procedure CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                  out Flaw: WideString); override;
    function CloneSignal(const AReader: TXmlCustomReader): TXmlSignal; override;
    function Scope: TXmlSignalScope; override;

    property Prefix: WideString read FPrefix write FPrefix;       // xxx necessary?
  end;

  TXmlEntityRefSignal = class(TXmlSignal)
  private
    FEntityName: WideString;
  public
    procedure CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                  out Flaw: WideString); override;
    function CloneSignal(const AReader: TXmlCustomReader): TXmlSignal; override;
    function Scope: TXmlSignalScope; override;

    property EntityName: WideString read FEntityName write FEntityName;
  end;

  TXmlPCDATASignal = class(TXmlSignal)
  private
    FCharRefGenerated: Boolean;
    FData: WideString;
  public
    constructor Create(const AReader: TXmlCustomReader;
                       const AStartByteNumber,
                             AStartCharNumber,
                             AStartColumnNumber,
                             AStartLineNumber,
                             AEndByteNumber,
                             AEndCharNumber,
                             AEndColumnNumber,
                             AEndLineNumber: Int64;
                       const AUri: wideString;
                       const ARelatedASObject: TdomASObject;
                       const ARelatedNode: TdomNode); override;
    procedure CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                  out Flaw: WideString); override;
    function CloneSignal(const AReader: TXmlCustomReader): TXmlSignal; override;
    function Scope: TXmlSignalScope; override;

    property CharRefGenerated: Boolean read FCharRefGenerated write FCharRefGenerated default False;
    property Data: WideString read FData write FData;
  end;

  TXmlSkippedEntitySignal = class(TXmlSignal)
  private
    FEntityName: WideString;
  public
    function CloneSignal(const AReader: TXmlCustomReader): TXmlSignal; override;
    function Scope: TXmlSignalScope; override;

    property EntityName: WideString read FEntityName write FEntityName;
  end;

  TXmlStartDocumentSignal = class(TXmlSignal)
  private
    FEncodingName: WideString;
    FInputEncoding: WideString;
    FStandaloneDecl: TdomStandalone;
    FVersion: WideString;
  public
    procedure CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                  out Flaw: WideString); override;
    function CloneSignal(const AReader: TXmlCustomReader): TXmlSignal; override;
    function Scope: TXmlSignalScope; override;

    property EncodingName: WideString read FEncodingName write FEncodingName;
    property InputEncoding: WideString read FInputEncoding write FInputEncoding;
    property StandaloneDecl: TdomStandalone read FStandaloneDecl write FStandaloneDecl;
    property Version: WideString read FVersion write FVersion;
  end;

  TXmlStartDocumentFragmentSignal = class(TXmlSignal)
  private
    FEncodingName: WideString;
  public
    procedure CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                  out Flaw: WideString); override;
    function CloneSignal(const AReader: TXmlCustomReader): TXmlSignal; override;
    function Scope: TXmlSignalScope; override;

    property EncodingName: WideString read FEncodingName write FEncodingName;
  end;

  TXmlStartElementSignal = class(TXmlSignal)
  private
    FAttributes: TUtilsNameValueList;
    FTagName: WideString;
    procedure SetAttributes(const Value: TUtilsNameValueList);
  public
    constructor Create(const AReader: TXmlCustomReader;
                       const AStartByteNumber,
                             AStartCharNumber,
                             AStartColumnNumber,
                             AStartLineNumber,
                             AEndByteNumber,
                             AEndCharNumber,
                             AEndColumnNumber,
                             AEndLineNumber: Int64;
                       const AUri: wideString;
                       const ARelatedASObject: TdomASObject;
                       const ARelatedNode: TdomNode); override;
    destructor Destroy; override;
    procedure CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                  out Flaw: WideString); override;
    function CloneSignal(const AReader: TXmlCustomReader): TXmlSignal; override;
    function Scope: TXmlSignalScope; override;

    property Attributes: TUtilsNameValueList read FAttributes write SetAttributes;
    property TagName: WideString read FTagName write FTagName;
  end;

  TXmlStartPrefixMappingSignal = class(TXmlSignal)
  private
    FPrefix: WideString;
    FUri: WideString;
  public
    procedure CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                  out Flaw: WideString); override;
    function CloneSignal(const AReader: TXmlCustomReader): TXmlSignal; override;
    function Scope: TXmlSignalScope; override;

    property Prefix: WideString read FPrefix write FPrefix;
    property Uri: WideString read FUri write FUri;
  end;

  { Non-DTD as well as DTD XML Signal }

  TXmlCommentSignal = class(TXmlSignal)
  private
    FData: WideString;
  public
    procedure CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                  out Flaw: WideString); override;
    function CloneSignal(const AReader: TXmlCustomReader): TXmlSignal; override;
    function Scope: TXmlSignalScope; override;

    property Data: WideString read FData write FData;
  end;

  TXmlProcessingInstructionSignal = class(TXmlSignal)
  private
    FData: WideString;
    FTarget: WideString;
  public
    procedure CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                  out Flaw: WideString); override;
    function CloneSignal(const AReader: TXmlCustomReader): TXmlSignal; override;
    function Scope: TXmlSignalScope; override;

    property Data: WideString read FData write FData;
    property Target: WideString read FTarget write FTarget;
  end;

  { DTD XML Signals }

  TXmlAttributeDefinitionSignal = class(TXmlSignal)
  private
    FAttributeName: WideString;
    FAttributeType: TXmlDataType;
    FConstraint: TdomAttrValueConstraint;
    FDefaultValue: WideString;
    FEnumeration: TUtilsWideStringList;
    FElementName: WideString;
    procedure SetEnumeration(const Value: TUtilsWideStringList);
  public
    constructor Create(const AReader: TXmlCustomReader;
                       const AStartByteNumber,
                             AStartCharNumber,
                             AStartColumnNumber,
                             AStartLineNumber,
                             AEndByteNumber,
                             AEndCharNumber,
                             AEndColumnNumber,
                             AEndLineNumber: Int64;
                       const AUri: wideString;
                       const ARelatedASObject: TdomASObject;
                       const ARelatedNode: TdomNode); override;
    destructor Destroy; override;
    procedure CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                  out Flaw: WideString); override;
    function CloneSignal(const AReader: TXmlCustomReader): TXmlSignal; override;
    function Scope: TXmlSignalScope; override;

    property AttributeName: WideString read FAttributeName write FAttributeName;
    property AttributeType: TXmlDataType read FAttributeType write FAttributeType;
    property Constraint: TdomAttrValueConstraint read FConstraint write FConstraint;
    property DefaultValue: WideString read FDefaultValue write FDefaultValue;
    property ElementName: WideString read FElementName write FElementName;
    property Enumeration: TUtilsWideStringList read FEnumeration write SetEnumeration;
  end;

  TXmlElementTypeDeclarationSignal = class(TXmlSignal)
  private
    FElementName: WideString;
    FData: WideString;
  public
    procedure CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                  out Flaw: WideString); override;
    function CloneSignal(const AReader: TXmlCustomReader): TXmlSignal; override;
    function Scope: TXmlSignalScope; override;

    property Data: WideString read FData write FData;
    property ElementName: WideString read FElementName write FElementName;
  end;

  TXmlEntityDeclarationSignal = class(TXmlSignal)
  private
    FEntityValue: WideString;
    FPublicId: WideString;
    FNotationName: WideString;
    FEntityName: WideString;
    FSystemId: WideString;
  public
    procedure CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                  out Flaw: WideString); override;
    function CloneSignal(const AReader: TXmlCustomReader): TXmlSignal; override;
    function Scope: TXmlSignalScope; override;

    property EntityName: WideString read FEntityName write FEntityName;
    property EntityValue: WideString read FEntityValue write FEntityValue;
    property NotationName: WideString read FNotationName write FNotationName;
    property PublicId: WideString read FPublicId write FPublicId;
    property SystemId: WideString read FSystemId write FSystemId;
  end;

  TXmlNotationDeclarationSignal = class(TXmlSignal)
  private
    FNotationName: WideString;
    FPublicId: WideString;
    FSystemId: WideString;
  public
    procedure CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                  out Flaw: WideString); override;
    function CloneSignal(const AReader: TXmlCustomReader): TXmlSignal; override;
    function Scope: TXmlSignalScope; override;

    property NotationName: WideString read FNotationName write FNotationName;
    property PublicId: WideString read FPublicId write FPublicId;
    property SystemId: WideString read FSystemId write FSystemId;
  end;

  TXmlParameterEntityDeclarationSignal = class(TXmlSignal)
  private
    FEntityName: WideString;
    FEntityValue: WideString;
    FPublicId: WideString;
    FSystemId: WideString;
  public
    procedure CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                  out Flaw: WideString); override;
    function CloneSignal(const AReader: TXmlCustomReader): TXmlSignal; override;
    function Scope: TXmlSignalScope; override;

    property EntityName: WideString read FEntityName write FEntityName;
    property EntityValue: WideString read FEntityValue write FEntityValue;
    property PublicId: WideString read FPublicId write FPublicId;
    property SystemId: WideString read FSystemId write FSystemId;
  end;

  TXmlStartExtDtdSignal = class(TXmlSignal)
  private
    FEncodingName: WideString;
    FInputEncoding: WideString;
    FPublicId: WideString;
    FSystemId: WideString;
    FVersion: WideString;
  public
    procedure CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                  out Flaw: WideString); override;
    function CloneSignal(const AReader: TXmlCustomReader): TXmlSignal; override;
    function Scope: TXmlSignalScope; override;

    property EncodingName: WideString read FEncodingName write FEncodingName;
    property InputEncoding: WideString read FInputEncoding write FInputEncoding;
    property PublicId: WideString read FPublicId write FPublicId;
    property SystemId: WideString read FSystemId write FSystemId;
    property Version: WideString read FVersion write FVersion;
  end;

  TXmlStartIntDtdSignal = class(TXmlSignal)
  private
    FSystemId: WideString;
  public
    function CloneSignal(const AReader: TXmlCustomReader): TXmlSignal; override;
    function Scope: TXmlSignalScope; override;
    property SystemId: WideString read FSystemId write FSystemId;
  end;

{ XML Reader Components }

  TXmlCustomReader = class(TDomBaseComponent)
  private
    FDOMImpl: TDomImplementation;
    FNextHandler: TXmlCustomHandler;
    FOnError: TdomErrorNotifyEvent;
    function getTabWidth: integer;
    procedure setDomImpl(const impl: TDomImplementation);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure sendErrorNotification(const xmlErrorType: TXmlErrorType;
                                    const location: IDomLocator;
                                    const code: wideString); virtual;
  public
    constructor create(AOwner: TComponent); override;
  published
    property DOMImpl: TDomImplementation read FDomImpl write setDomImpl;
    property NextHandler: TXmlCustomHandler read FNextHandler write FNextHandler;
    property TabWidth: integer read getTabWidth;

    property OnError: TdomErrorNotifyEvent read FOnError write FOnError;
  end;

  TXmlStandardDocReader = class (TXmlCustomReader)
  protected
    FPrefixMapping: boolean;
    FPrefixMappingStack: TList;
    FSuppressXmlns: boolean;
    procedure clearPrefixMappingStack; virtual;
    procedure parse2(const xmlTokenizer: TXmlDocTokenizer); virtual;
    procedure sendAbortedSignal(const locator: IDomLocator); virtual;
    procedure writeCDATA(const locator: IDomLocator;
                         const content: wideString); virtual;
    procedure writeCharRefDec(const locator: IDomLocator;
                              const content: wideString); virtual;
    procedure writeCharRefHex(const locator: IDomLocator;
                              const content: wideString); virtual;
    procedure writeComment(const locator: IDomLocator;
                           const content: wideString); virtual;
    procedure writePCDATA(const locator: IDomLocator;
                          const content: wideString); virtual;
    procedure writeProcessingInstruction(const locator: IDomLocator;
                                         const content: wideString); virtual;
    procedure writeStartDocument(const locator: IDomLocator;
                                 const inputEnc,
                                       version,
                                       encName: wideString;
                                       sdDl: TdomStandalone); virtual;
    procedure writeStartDocumentFragment(const locator: IDomLocator;
                                         const encName: wideString); virtual;
    procedure writeStartElement(const locator: IDomLocator;
                                const content: wideString); virtual;
    procedure writeStartElementSimple(const locator: IDomLocator;
                                            content: wideString;
                                        out tagName: wideString;
                                      const attrList: TUtilsNameValueList); virtual;
    procedure writeStartElementXmlns(const locator: IDomLocator;
                                           content: wideString;
                                       out tagName: wideString;
                                     const attrList: TUtilsNameValueList); virtual;
    procedure writeStartPrefixMapping(const locator: IDomLocator;
                                      const prefix,
                                            uri: wideString); virtual;
    procedure writeEndElement(const locator: IDomLocator;
                              const content: wideString); virtual;
    procedure writeEndPrefixMapping(const locator: IDomLocator); virtual;
    procedure writeEmptyElement(const locator: IDomLocator;
                                const content: wideString); virtual;
    procedure writeEntityRef(const locator: IDomLocator;
                             const content: wideString); virtual;
    procedure writeDoctype(const locator: IDomLocator;
                           const content: wideString); virtual;
    procedure writeCompleted(const locator: IDomLocator); virtual;
  public
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    function  parse(const inputSource: TXmlInputSource): boolean; virtual;
    function  parseFragment(const inputSource: TXmlInputSource): boolean; virtual;
  published
    property PrefixMapping: boolean read FPrefixMapping write FPrefixMapping;
    property SuppressXmlns: boolean read FSuppressXmlns write FSuppressXmlns;
  end;

  TXmlStandardDtdReader = class (TXmlCustomReader)
  private
    FAttrListDeclNames: TUtilsWideStringList; // List to record the element types of
                                              // attribute-list declarations to detect
                                              // duplicates.
    FElementTypeDeclNames: TUtilsWideStringList; // List to record element types
                                                 // declarations to detect
                                                 // duplicates.
    FPERepository: TdomPERepository; // Collection of parameter entities.
    FPERefTreatment: TdomPERefTreatment;
    FXmlErrorDetected: Boolean;
    function findNextAttDef(    decl: wideString;
                            out attType: TXmlDataType;
                            out constraint: TdomAttrValueConstraint;
                            out attName,
                                enumeration,
                                defaultValue,
                                rest: wideString): boolean;
    procedure parseloop(const Tokenizer: TXmlCustomSubsetTokenizer);
  protected
    procedure sendAbortedSignal(const locator: IDomLocator); virtual;
    procedure sendErrorNotification(const xmlErrorType: TXmlErrorType;
                                    const location: IDomLocator;
                                    const code: wideString); override;
    procedure writeAttributeDeclaration(const locator: IDomLocator;
                                        const elementName: wideString;
                                              data: wideString); virtual;
    procedure writeDTDComment(const locator: IDomLocator;
                              const data: wideString); virtual;
    procedure writeDTDProcessingInstruction(const locator: IDomLocator;
                                            const target,
                                                  data: wideString); virtual;
    procedure writeElementDeclaration(const locator: IDomLocator;
                                      const elementName,
                                            data: wideString); virtual;
    procedure writeEntityDeclaration(const locator: IDomLocator;
                                     const entityName,
                                           data: wideString); virtual;
    procedure writeCompleted(const locator: IDomLocator); virtual;
    procedure writeNotationDeclaration(const locator: IDomLocator;
                                       const notationName: wideString;
                                             data: wideString); virtual;
    procedure writeParameterEntityDeclaration(const locator: IDomLocator;
                                              const entityName,
                                                    data: wideString); virtual;
    procedure writeStartExtDtd(const locator: IDomLocator;
                               const inputEnc,
                                     pubId,
                                     sysId,
                                     version,
                                     encName: wideString); virtual;
    procedure writeStartIntDtd(const locator: IDomLocator;
                               const sysId: wideString); virtual;
  public
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    function  parseExternalSubset(const inputSource: TXmlInputSource): boolean; virtual;
    function  parseInternalSubset(const inputSource: TXmlInputSource): boolean; virtual;
    procedure prepare; virtual;

    property PERefTreatment: TdomPERefTreatment read FPERefTreatment write FPERefTreatment default PT_PARSE;
  end;

  TXmlStandardDomReader = class (TXmlCustomReader)
  private
    FContextNode: TdomNode;
    FIgnoreUnspecified: boolean;
    FPrefixMapping: boolean;
    FSuppressXmlns: boolean;
  protected
    function getContextNode: TdomNode; virtual;
    function getSystemId: wideString; virtual;
    procedure parseloop(const sourceNode: TdomNode); virtual;
    procedure sendAbortedSignal; virtual;
    procedure writeCDATA(const content: wideString); virtual;
    procedure writeComment(const content: wideString); virtual;
    procedure writeDoctype(const doctypeName,
                                 publicId,
                                 systemId,
                                 intSubset: wideString); virtual;
    procedure writeEndElement(const tagName: wideString); virtual;
    procedure writeEndPrefixMapping(const prefix: wideString); virtual;
    procedure writeEntityRef(const entityName: wideString); virtual;
    procedure writeCompleted; virtual;
    procedure writePCDATA(const content: wideString); virtual;
    procedure writeProcessingInstruction(const targ,
                                               attribSequence : wideString); virtual;
    procedure writeStartDocument(const inputEnc,
                                       version,
                                       encName: wideString;
                                       sdDl: TdomStandalone); virtual;
    procedure writeStartDocumentFragment(const encName: wideString); virtual;
    procedure writeStartElement(const tagName: wideString;
                                const attributeList: TUtilsNameValueList); virtual;
    procedure writeStartPrefixMapping(const prefix,
                                            uri: wideString); virtual;
  public
    constructor create(AOwner: TComponent); override;
    function parse(const sourceNode: TdomNode): boolean; virtual;
    property contextNode: TdomNode read getContextNode;
  published
    property IgnoreUnspecified: boolean read FIgnoreUnspecified write FIgnoreUnspecified;
    property PrefixMapping: boolean read FPrefixMapping write FPrefixMapping;
    property SuppressXmlns: boolean read FSuppressXmlns write FSuppressXmlns;
  end;

{ XML Content Handler Components }

  TXmlCustomHandler = class(TDomBaseComponent)
  protected
    procedure sendErrorNotification(const target: TXmlCustomReader;
                                    const xmlErrorType: TXmlErrorType;
                                    const location: IDomLocator;
                                    const code: wideString); virtual;
  public
    procedure processSignal(const signal: TXmlSignal); virtual; abstract;
  end;

  TXmlStandardHandler = class(TXmlCustomHandler)
  protected
    FNextHandler: TXmlCustomHandler;
    FOnSignal: TXmlProcessingEvent;
    FOnSignaled: TXmlPostProcessingEvent;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure processSignal(const signal: TXmlSignal); override;
  published
    property NextHandler: TXmlCustomHandler read FNextHandler write FNextHandler;

    property OnSignal: TXmlProcessingEvent read FOnSignal write FOnSignal;
    property OnSignaled: TXmlPostProcessingEvent read FOnSignaled write FOnSignaled;
  end;

  TXmlDistributor = class;

  TXmlHandlerItem = class(TCollectionItem)
  protected
    FXmlHandler: TXmlCustomHandler;
    function getXmlHandler: TXmlCustomHandler;
    procedure setXmlHandler(Value: TXmlCustomHandler);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property XmlHandler: TXmlCustomHandler read getXmlHandler write setXmlHandler;
  end;

  TXmlHandlers = class(TCollection)
  private
    FDistributor: TXmlDistributor;
  protected
    function GetItem(Index: Integer): TXmlHandlerItem; virtual;
    procedure SetItem(Index: Integer; Value: TXmlHandlerItem); virtual;
    function GetOwner: TPersistent; override;
  public
    constructor Create(Distributor: TXmlDistributor);
    function Add: TXmlHandlerItem;
    procedure Assign(Source: TPersistent); override;
    function FindHandlerItem(AHandler: TXmlCustomHandler): TXmlHandlerItem;
    property Distributor: TXmlDistributor read FDistributor;
    property Items[Index: Integer]: TXmlHandlerItem read GetItem write SetItem; default;
  end;

  TXmlDistributor = class(TXmlCustomHandler)
  private
    FDisableCount: Integer;
    procedure readData(Reader: TReader);
    procedure writeData(Writer: TWriter);
  protected
    FNextHandlers: TXmlHandlers;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure setNextHandlers(const value: TXmlHandlers);
  public
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure processSignal(const signal: TXmlSignal); override;
  published
    property NextHandlers: TXmlHandlers read FNextHandlers write setNextHandlers;
  end;

  TXmlActivityStatus = ( asInactive, asDocActive, asDocFragActive, asExtDtdActive, asIntDtdActive );

  TXmlRootProcessingStatus = (rsBeforeRoot, rsInRoot, rsAfterRoot);

  TXmlWFTestHandler = class(TXmlCustomHandler)
  protected
    FActivityStatus: TXmlActivityStatus;
    FDoctypeFound: boolean;
    FNextHandler: TXmlCustomHandler;
    FPrefixStack: TUtilsWideStringList;
    FRootProcessingStatus: TXmlRootProcessingStatus;
    FTagStack: TUtilsWideStringList;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure reset; virtual;
  public
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure processSignal(const signal: TXmlSignal); override;

    property activityStatus: TXmlActivityStatus read FActivityStatus;
  published
    property NextHandler: TXmlCustomHandler read FNextHandler write FNextHandler;
  end;

  TXmlDomBuilder = class(TXmlCustomHandler)
  private
    FAutoPrepare: TdomAutoPrepare;
    FBuildNamespaceTree: boolean;
    FKeepCDATASections: boolean;
    FKeepComments: boolean;
    FKeepDocumentTypeDecl: boolean;
    FKeepEntityRefs: boolean;
  protected
    FRefNode: TdomNode;
    FPrefixUriList: TUtilsNameValueList;
    procedure writePCDATA(const sender: TXmlCustomReader;
                          const locator: IDomLocator;
                          const data: wideString); virtual;
    procedure reset; virtual;
  public
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure processSignal(const signal: TXmlSignal); override;

    property referenceNode: TdomNode read FRefNode write FRefNode;
  published
    property AutoPrepare: TdomAutoPrepare read FAutoPrepare write FAutoPrepare default AP_NO;
    property BuildNamespaceTree: boolean read FBuildNamespaceTree write FBuildNamespaceTree default false;
    property KeepCDATASections: boolean read FKeepCDATASections write FKeepCDATASections default true;
    property KeepComments: boolean read FKeepComments write FKeepComments default true;
    property KeepDocumentTypeDecl: boolean read FKeepDocumentTypeDecl write FKeepDocumentTypeDecl default true;
    property KeepEntityRefs: boolean read FKeepEntityRefs write FKeepEntityRefs default true;
  end;

  TXmlASBuilder = class(TXmlCustomHandler)
  private
    FASModel: TdomASModel;
  protected
    procedure insertMixedContent(const sender: TXmlCustomReader;
                                 const refASElementDecl: TdomASElementDecl;
                                 const contSpec: wideString); virtual;
    procedure insertChildrenContent(const sender: TXmlCustomReader;
                                    const refASObject: TdomASObject;
                                    const contSpec: wideString); virtual;
    procedure setASModel(const value: TdomASModel); virtual;
  public
    constructor create(AOwner: TComponent); override;
    procedure processSignal(const signal: TXmlSignal); override;

    property ASModel: TdomASModel read FASModel write setASModel;
  end;

  TXmlStreamBuilder = class(TXmlCustomHandler)
  private
    FAttListDeclIsOpen: boolean; // Remark: If this variable is to be "published" the CheckAttListDeclarationOpen method must be modified accordingly!
    FByteCount: integer;
    FCharacterCount: integer;
    FColumnCount: integer;
    FCurrentAttListDeclName: wideString;
    FCurrentEncoding: wideString;
    FDefaultEncoding: wideString;
    FDefaultCodecClass: TUnicodeCodecClass;
    FIncludeXmlDecl: boolean;
    FLineFeedCount: integer;
    FTabCount: integer;
    FUseByteOrderMark: boolean;
    FOutputSource: TXmlOutputSource;
    FOnAfterWrite: TdomSerializationEvent;
    FOnBeforeWrite: TdomSerializationEvent;
    procedure CheckAttListDeclarationClosed(const Sender: TXmlCustomReader;
                                            const Locator: IDomLocator);
    procedure CheckAttListDeclarationOpen(const sender: TXmlCustomReader;
                                          const locator: IDomLocator;
                                          const elementName: wideString);
    function getCurrentCodecClass: TUnicodeCodecClass;
    procedure putCurrentCodecClass(const value: TUnicodeCodecClass);
    procedure resetCurrentCodecClass;
    procedure setDefaultEncoding(const value: wideString);
    procedure setOutputSource(const value: TXmlOutputSource);
    procedure writeWideString(const S: wideString;
                              const useCharRefs: boolean);

    procedure WriteAttributeDefinitionSignal(const Signal: TXmlAttributeDefinitionSignal);
    procedure WriteCDATASignal(const Signal: TXmlCDataSignal);
    procedure WriteCommentSignal(const Signal: TXmlCommentSignal);
    procedure WriteDoctypeSignal(const Signal: TXmlDoctypeSignal);
    procedure WriteElementTypeDeclarationSignal(const Signal: TXmlElementTypeDeclarationSignal);
    procedure WriteEndElementSignal(const Signal: TXmlEndElementSignal);
    procedure WriteEntityDeclarationSignal(const Signal: TXmlEntityDeclarationSignal);
    procedure WriteEntityRefSignal(const Signal: TXmlEntityRefSignal);
    procedure WriteCompletedSignal(const Signal: TXmlCompletedSignal);
    procedure WriteNotationDeclarationSignal(const Signal: TXmlNotationDeclarationSignal);
    procedure WriteParameterEntityDeclarationSignal(const Signal: TXmlParameterEntityDeclarationSignal);
    procedure WritePCDATASignal(const Signal: TXmlPCDATASignal);
    procedure WriteProcessingInstructionSignal(const Signal: TXmlProcessingInstructionSignal);
    procedure WriteSkippedEntitySignal(const Signal: TXmlSkippedEntitySignal);
    procedure WriteStartDocumentSignal(const Signal: TXmlStartDocumentSignal);
    procedure WriteStartDocumentFragmentSignal(const Signal: TXmlStartDocumentFragmentSignal);
    procedure WriteStartElementSignal(const Signal: TXmlStartElementSignal);
    procedure WriteStartExtDtdSignal(const Signal: TXmlStartExtDtdSignal);
    procedure WriteStartIntDtdSignal(const Signal: TXmlStartIntDtdSignal);
  protected
    FOpenElementsCount: integer;
    procedure doAfterWrite(const pieceType: TdomPieceType;
                           const locator: IDomLocator);
    procedure doBeforeWrite(const pieceType: TdomPieceType;
                            const locator: IDomLocator);
    procedure reset; virtual;
    procedure setIncludeXmlDecl(const value: boolean); virtual;
    procedure setUseByteOrderMark(const value: boolean); virtual;
    procedure writeByteOrderMark(const sender: TXmlCustomReader;
                                 const locator: IDomLocator;
                                   out byteCount: Integer); virtual;
    procedure writeWideStrings(const sender: TXmlCustomReader;
                               const locator: IDomLocator;
                               const xmlStrgs: array of wideString;
                               const useCharRefs: boolean); virtual;
  public
    constructor create(aOwner: TComponent); override;
    procedure processSignal(const signal: TXmlSignal); override;

    property byteCount: integer read FByteCount;            // xxx Revisit.
    property characterCount: integer read FCharacterCount;  // xxx Revisit.
    property columnCount: integer read FColumnCount;        // xxx Revisit.
    property currentEncoding: wideString read FCurrentEncoding;
    property currentCodecClass: TUnicodeCodecClass read GetCurrentCodecClass;
    property defaultEncoding: wideString read FDefaultEncoding write SetDefaultEncoding;
    property defaultCodecClass: TUnicodeCodecClass read FDefaultCodecClass;
    property lineFeedCount: integer read FLineFeedCount;    // xxx Revisit.
    property outputSource: TXmlOutputSource read FOutputSource write setOutputSource;
    property tabCount: integer read FTabCount;
  published
    property UseByteOrderMark: boolean read FUseByteOrderMark write setUseByteOrderMark default false;
    property IncludeXmlDecl: boolean read FIncludeXmlDecl write setIncludeXmlDecl default true;

    property OnAfterWrite: TdomSerializationEvent read FOnAfterWrite write FOnAfterWrite;
    property OnBeforeWrite: TdomSerializationEvent read FOnBeforeWrite write FOnBeforeWrite; 
  end;

  TXmlCustomParser = class(TDomBaseComponent)
  private
    FDOMImpl: TDomImplementation;
  protected
    procedure setDomImpl(const impl: TDomImplementation); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    constructor create(aOwner: TComponent); override;
    property DOMImpl: TDomImplementation read FDOMImpl write setDomImpl;
  end;

  TXmlToDomParser = class (TXmlCustomParser)
  private
    function getKeepCDATASections: boolean;
    function getKeepComments: boolean;
    function getKeepEntityRefs: boolean;
    procedure setKeepCDATASections(const value: boolean);
    procedure setKeepComments(const value: boolean);
    procedure setKeepEntityRefs(const value: boolean);
    function getAutoPrepare: TdomAutoPrepare;
    procedure setAutoPrepare(const Value: TdomAutoPrepare);
  protected
    FBufferSize: Integer;
    FDocBuilder: TXmlDomBuilder;
    FDocReader:  TXmlStandardDocReader;
    FNewDoc: TdomDocument;  // Internal buffer for new documents.
    FWFTestHandler: TXmlWFTestHandler;
    procedure createSubcomponents; virtual;
    function sendErrorNotification(const xmlErrorType: TXmlErrorType): boolean; virtual;
    procedure setBufferSize(const value: integer); virtual;
    procedure setDomImpl(const impl: TDomImplementation); override;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    function fileToDom(const filename: TFileName): TdomDocument; virtual;
    function parseSourceCode(const docSourceCode: TXmlSourceCode;
                             const pubId,
                                   sysId: wideString;
                             const refNode: TdomNode): TdomNode; virtual;
    function parseStream(const stream: TStream;
                         const pubId,
                               sysId: wideString;
                         const refNode: TdomNode): TdomNode; virtual;
    function parseString(const str: string;
                         const pubId,
                               sysId: wideString;
                         const refNode: TdomNode): TdomNode; virtual;
    function parseURI(      pubId,
                            sysId: wideString;
                      const refNode: TdomNode): TdomNode; virtual;
    function parseWideString(const str: wideString;
                             const pubId,
                                   sysId: wideString;
                             const refNode: TdomNode): TdomNode; virtual;
    function parseXmlInputSource(const inputSource: TXmlInputSource;
                                 const refNode: TdomNode): TdomNode; virtual;
  published
    property AutoPrepare: TdomAutoPrepare read getAutoPrepare write setAutoPrepare default AP_NO;  // xxx Change this for automatic standalone detection?
    property BufferSize: integer read FBufferSize write setBufferSize default 4096;
    property KeepCDATASections: boolean read getKeepCDATASections write setKeepCDATASections default true;
    property KeepComments: boolean read getKeepComments write setKeepComments default true;
    property KeepEntityRefs: boolean read getKeepEntityRefs write setKeepEntityRefs default true;
  end;

  TDtdToASParser = class (TXmlCustomParser)
  private
    FBufferSize: Integer;
  protected
    FASBuilder: TXmlASBuilder;
    FDtdReader: TXmlStandardDtdReader;
    FWFTestHandler: TXmlWFTestHandler;
    procedure createSubcomponents; virtual;
    function getPERefTreatment: TdomPERefTreatment; virtual;
    function sendErrorNotification(const xmlErrorType: TXmlErrorType): boolean; virtual;
    procedure setBufferSize(const value: integer); virtual;
    procedure setDomImpl(const impl: TDomImplementation); override;
    procedure setPERefTreatment(const Value: TdomPERefTreatment); virtual;
  public
    constructor create(aOwner: TComponent); override;
    procedure extDtdSourceCodeToAS(const ExtDtdSourceCode: TXmlSourceCode;
                                   const pubId,
                                         sysId: wideString;
                                   const target: TdomASModel); virtual;   // xxx change target type to TdomASModelCollection. Ditto for the other procedures.
    procedure extDtdStreamToAS(const stream: TStream;
                               const pubId,
                                     sysId: wideString;
                               const target: TdomASModel); virtual;
    procedure extDtdStringToAS(const str: string;
                               const pubId,
                                     sysId: wideString;
                               const target: TdomASModel); virtual;
    procedure extDtdWideStringToAS(      str: wideString;
                                   const pubId,
                                         sysId: wideString;
                                   const target: TdomASModel); virtual;
    procedure intDtdSourceCodeToAS(const intDtdSourceCode: TXmlSourceCode;
                                   const pubId,
                                         sysId: wideString;
                                   const intSubsetStartByteNumber,
                                         intSubsetStartCharNumber,
                                         intSubsetStartColumn,
                                         intSubsetStartLine: Int64;
                                   const target: TdomASModel); virtual;
    procedure intDtdStreamToAS(const stream: TStream;
                               const pubId,
                                     sysId: wideString;
                               const intSubsetStartByteNumber,
                                     intSubsetStartCharNumber,
                                     intSubsetStartColumn,
                                     intSubsetStartLine: Int64;
                               const target: TdomASModel); virtual;
    procedure intDtdStringToAS(const str: string;
                               const pubId,
                                     sysId: wideString;
                               const intSubsetStartByteNumber,
                                     intSubsetStartCharNumber,
                                     intSubsetStartColumn,
                                     intSubsetStartLine: Int64;
                               const target: TdomASModel); virtual;
    procedure intDtdWideStringToAS(      str: wideString;
                                   const pubId,
                                         sysId: wideString;
                                   const intSubsetStartByteNumber,
                                         intSubsetStartCharNumber,
                                         intSubsetStartColumn,
                                         intSubsetStartLine: Int64;
                                   const target: TdomASModel); virtual;
    function parseDTD(const intSubset,
                            docUri: wideString;
                      const intSubsetStartByteNumber,
                            intSubsetStartCharNumber,
                            intSubsetStartColumn,
                            intSubsetStartLine: Int64;
                            pubId,
                            sysId: wideString;
                      const ASModelCollection: TdomASModelCollection): boolean; virtual;
    procedure prepare; virtual;
  published
    property BufferSize: Integer read FBufferSize write setBufferSize default 4096;
    property PERefTreatment: TdomPERefTreatment read getPERefTreatment write setPERefTreatment default PT_PARSE;
  end;

  TDomToXmlParser = class (TXmlCustomParser)
  private
    FDomReader: TXmlStandardDomReader;
    FBufferSize: Integer;
    FStreamBuilder: TXmlStreamBuilder;
    FUseActiveCodePage: boolean;
    FWFTestHandler: TXmlWFTestHandler;
    FWriteLFOption: TCodecWriteLFOption;
    function getOnAfterWrite: TdomSerializationEvent;
    function getOnBeforeWrite: TdomSerializationEvent;
    function getStrictErrorChecking: boolean;
    function getUseByteOrderMark: boolean;
    procedure setOnAfterWrite(const Value: TdomSerializationEvent);
    procedure setOnBeforeWrite(const Value: TdomSerializationEvent);
    procedure setStrictErrorChecking(const value: boolean);
    procedure setUseByteOrderMark(const value: boolean);
  protected
    function getIgnoreUnspecified: boolean; virtual;
    function getIncludeXmlDecl: boolean; virtual;
    procedure setBufferSize(const value: integer); virtual;
    procedure setIgnoreUnspecified(const value: boolean); virtual;
    procedure setIncludeXmlDecl(const value: boolean); virtual;
    {$IFNDEF LINUX}
    procedure setUseActiveCodePage(const value: boolean); virtual; {$IFDEF VER140+} platform; {$ENDIF}
    {$ENDIF}
    procedure setWriteLFOption(const value: TCodecWriteLFOption); virtual;

    property DomReader: TXmlStandardDomReader read FDomReader;
    property StreamBuilder: TXmlStreamBuilder read FStreamBuilder;
    property WFTestHandler: TXmlWFTestHandler read FWFTestHandler;
  public
    constructor create(aOwner: TComponent); override;
    function writeToStream(const wnode: TdomNode;
                           const encoding: wideString;
                           const destination: TStream): boolean; virtual;
    function writeToString(const wnode: TdomNode;
                                 encoding: wideString;
                             out S: string): boolean; virtual;
    function writeToWideString(const wnode: TdomNode;
                                 out S: wideString): boolean; virtual;
  published
    property BufferSize: integer read FBufferSize write setBufferSize default 4096;
    property IgnoreUnspecified: boolean read getIgnoreUnspecified write setIgnoreUnspecified;
    property IncludeXmlDecl: boolean read getIncludeXmlDecl write setIncludeXmlDecl default true;
    property StrictErrorChecking: boolean read getStrictErrorChecking write setStrictErrorChecking default false;
    {$IFNDEF LINUX}
    property UseActiveCodePage: boolean read FUseActiveCodePage write setUseActiveCodePage default false;
    {$ENDIF}
    property UseByteOrderMark: boolean read getUseByteOrderMark write setUseByteOrderMark;
    property WriteLFOption: TCodecWriteLFOption read FWriteLFOption write setWriteLFOption default lwCRLF;

    property OnAfterWrite: TdomSerializationEvent read getOnAfterWrite write setOnAfterWrite;
    property OnBeforeWrite: TdomSerializationEvent read getOnBeforeWrite write setOnBeforeWrite;
  end;


{XPath implementation}

  TdomXPathExpr  = class;

  TdomXPathTokenType = ( XPATH_LEFT_PARENTHESIS_TOKEN,
                         XPATH_RIGHT_PARENTHESIS_TOKEN,
                         XPATH_LEFT_SQUARE_BRACKET_TOKEN,
                         XPATH_RIGHT_SQUARE_BRACKET_TOKEN,
                         XPATH_SINGLE_DOT_TOKEN,
                         XPATH_DOUBLE_DOT_TOKEN,
                         XPATH_COMMERCIAL_AT_TOKEN,
                         XPATH_COMMA_TOKEN,
                         XPATH_DOUBLE_COLON_TOKEN,
                         XPATH_NAME_TEST_TOKEN,
                         XPATH_NODE_TYPE_COMMENT_TOKEN,
                         XPATH_NODE_TYPE_TEXT_TOKEN,
                         XPATH_NODE_TYPE_PI_TOKEN,
                         XPATH_NODE_TYPE_NODE_TOKEN,
                         XPATH_AND_OPERATOR_TOKEN,
                         XPATH_OR_OPERATOR_TOKEN,
                         XPATH_MOD_OPERATOR_TOKEN,
                         XPATH_DIV_OPERATOR_TOKEN,
                         XPATH_MULTIPLY_OPERATOR_TOKEN,
                         XPATH_SLASH_OPERATOR_TOKEN,
                         XPATH_SHEFFER_STROKE_OPERATOR_TOKEN,
                         XPATH_PLUS_OPERATOR_TOKEN,
                         XPATH_MINUS_OPERATOR_TOKEN,
                         XPATH_IS_EQUAL_OPERATOR_TOKEN,
                         XPATH_IS_NOT_EQUAL_OPERATOR_TOKEN,
                         XPATH_LESS_THAN_OPERATOR_TOKEN,
                         XPATH_LESS_THAN_OR_EQUAL_OPERATOR_TOKEN,
                         XPATH_GREATER_THAN_OPERATOR_TOKEN,
                         XPATH_GREATER_THAN_OR_EQUAL_OPERATOR_TOKEN,
                         XPATH_FUNCTION_NAME_TOKEN,
                         XPATH_AXIS_NAME_ANCESTOR_TOKEN,
                         XPATH_AXIS_NAME_ANCESTOR_OR_SELF_TOKEN,
                         XPATH_AXIS_NAME_ATTRIBUTE_TOKEN,
                         XPATH_AXIS_NAME_CHILD_TOKEN,
                         XPATH_AXIS_NAME_DESCENDANT_TOKEN,
                         XPATH_AXIS_NAME_DESCENDANT_OR_SELF_TOKEN,
                         XPATH_AXIS_NAME_FOLLOWING_TOKEN,
                         XPATH_AXIS_NAME_FOLLOWING_SIBLING_TOKEN,
                         XPATH_AXIS_NAME_NAMESPACE_TOKEN,
                         XPATH_AXIS_NAME_PARENT_TOKEN,
                         XPATH_AXIS_NAME_PRECEDING_TOKEN,
                         XPATH_AXIS_NAME_PRECEDING_SIBLING_TOKEN,
                         XPATH_AXIS_NAME_SELF_TOKEN,
                         XPATH_LITERAL_TOKEN,
                         XPATH_NUMBER_TOKEN,
                         XPATH_VARIABLE_REFERENCE_TOKEN,
                         XPATH_END_OF_TEXT_TOKEN,
                         XPATH_INVALID_TOKEN
                       );

  TdomXPathAxisType = ( XPATH_FORWARD_AXIS, XPATH_REVERSE_AXIS );

  TdomXPathFunction = function(const contextNode: TdomNode;
                               const contextPosition: Integer;
                               const contextSize: Integer;
                               const arguments: TList): TdomXPathCustomResult;

  TdomXPathSlashStatus = ( SL_NO_DOUBLE_SLASH,
                           SL_XPATH_AXIS_NAME_DESCENDANT_OR_SELF_TOKEN_FOLLOWS,
                           SL_XPATH_DOUBLE_COLON_TOKEN_FOLLOWS,
                           SL_XPATH_NODE_TYPE_NODE_TOKEN_FOLLOWS,
                           SL_XPATH_LEFT_PARENTHESIS_FOLLOWS,
                           SL_XPATH_RIGHT_PARENTHESIS_FOLLOWS,
                           SL_XPATH_SLASH_OPERATOR_TOKEN_FOLLLOWS );

  TdomXPathTokenizer = class
  protected
    FCacheIsActive: boolean;
    FDoubleSlashStatus: TdomXPathSlashStatus;
    FExpression: wideString;
    FLastSymbol: TdomXPathTokenType;
    FPosition: integer;
    FPositionCache: integer;
    FSymbolCache: TdomXPathTokenType;
    FValueCache: wideString;
    function doubleColonFollows: boolean; virtual;
    function getNextWideChar(out s: wideChar): boolean; virtual;
    function leftParanthesisFollows: boolean; virtual;
    function lookAheadNextWideChar(out s: wideChar): boolean; virtual;
  public
    constructor create(const expression: wideString;
                       const xpathVersion: wideString); virtual;
    function isFollowing(const symbol: TdomXPathTokenType): boolean; virtual;
    procedure read(out symbol: TdomXPathTokenType;
                   out value: wideString;
                   out position: integer); virtual;
    procedure reset; virtual;
  end;

  TdomXPathCustomResult = class(TCustomOwnedNode)
  protected
    function getAxisType: TdomXPathAxisType; virtual;
    procedure setAxisType(const Value: TdomXPathAxisType); virtual;
  public
    constructor create;
    function asBoolean: boolean; virtual; abstract;
    function asNumber: double; virtual; abstract;
    function asWideString: wideString; virtual; abstract;
    function item(const index: integer): TdomNode; virtual;
    function length: integer; virtual;
    function resultType: TdomXPathResultType; virtual; abstract;

    property axisType: TdomXPathAxisType read getAxisType write setAxisType;
  end;

  TdomXPathResultClass = class of TdomXPathCustomResult;

  TdomXPathNodeSetResult = class(TdomXPathCustomResult)
  private
    function createXPathNamespace(const aOwnerElement: TdomElement;
                                  const aNamespaceUri,
                                        aPrefix: wideString): TdomXPathNamespace;
  protected
    FAxisType: TdomXPathAxisType;
    FList: TList;
    procedure addXPathNamespace(const aOwnerElement: TdomElement;
                                const aNamespaceUri,
                                      aPrefix: wideString); virtual;
    function getAxisType: TdomXPathAxisType; override;
    procedure insert(const index: integer;
                     const node: TdomNode); virtual;
    procedure setAxisType(const value: TdomXPathAxisType); override;
  public
    constructor create; virtual;
    destructor destroy; override;
    procedure add(const node: TdomNode); virtual;
    procedure addSubtree(const node: TdomNode); virtual;
    function asBoolean: boolean; override;
    function asNumber: double; override;
    procedure Assign(Source: TPersistent); override;
    function asWideString: wideString; override;
    procedure clear; virtual;
    procedure Delete(const index: integer); virtual;
    function item(const index: integer): TdomNode; override;
    function length: integer; override;
    procedure merge(const nodeSet: TdomXPathNodeSetResult); virtual;
    function resultType: TdomXPathResultType; override;
  end;

  TdomXPathBooleanResult = class(TdomXPathCustomResult)
  private
    FBooleanValue: boolean;
  public
    constructor create(const aBooleanValue: boolean); virtual;
    function asBoolean: boolean; override;
    function asNumber: double; override;
    function asWideString: wideString; override;
    function resultType: TdomXPathResultType; override;
  end;

  TdomXPathNumberResult = class(TdomXPathCustomResult)
  private
    FNumberValue: double;
  public
    constructor create(const aNumberValue: double); virtual;
    function asBoolean: boolean; override;
    function asNumber: double; override;
    function asWideString: wideString; override;
    function resultType: TdomXPathResultType; override;
  end;

  TdomXPathStringResult = class(TdomXPathCustomResult)
  private
    FStringValue: wideString;
  public
    constructor create(const aStringValue: wideString); virtual;
    function asBoolean: boolean; override;
    function asNumber: double; override;
    function asWideString: wideString; override;
    function resultType: TdomXPathResultType; override;
  end;

  TdomXPathSyntaxTree = class(TCustomOwnedObject)
  private
    FOwnerXPathExpression: TXPathExpression;
  protected
    FRootExpr: TdomXPathExpr;
    function createSyntaxNode(const symbol: TdomXPathTokenType;
                              const value: wideString): TdomXPathSyntaxNode; virtual;
    function getContextNode: TdomNode; virtual;
    function getIsPrepared: boolean; virtual;
  public
    constructor create(aOwner: TXPathExpression);
    procedure clear; override;
    function evaluate: TdomXPathCustomResult; virtual;
    function prepare(const expression: wideString): boolean; virtual;
    property contextNode: TdomNode read getContextNode;
    property isPrepared: boolean read getIsPrepared;
    property ownerXPathExpression: TXPathExpression read FOwnerXPathExpression;
  end;

  TXPathExpression = class(TDomBaseComponent)
  protected
    FContextNode: TdomNode;              // The context node for this XPath expression.
    FIsValid: TdomTrinarean;             // Indicates whether the XPath expression is valid.
    FExpression: wideString;             // Holds the expression to be evaluated.
    FSyntaxTree: TdomXPathSyntaxTree;    // Holds the XPath syntax tree.
    FXPathResult: TdomXPathCustomResult; // Holds the result of the evaluation.
    procedure setContextNode(const node: TdomNode); virtual;
    procedure setExpression(const S: wideString); virtual;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    function acquireXPathResult(const resultType: TdomXPathResultClass): TdomXPathCustomResult; virtual;
    function evaluate: boolean; virtual;
    function hasNodeSetResult: boolean; virtual;
    function prepare: boolean; virtual;
    function resultAxisType: TdomXPathAxisType; virtual;
    function resultAsBoolean: boolean; virtual;
    function resultAsNumber: double; virtual;
    function resultAsWideString: wideString; virtual;
    function resultNode(const index: integer): TdomNode; virtual;
    function resultLength: integer; virtual;

    property contextNode: TdomNode read FContextNode write setContextNode;
    property isValid: TdomTrinarean read FIsValid;
  published
    property Expression: WideString read FExpression write setExpression;
  end;

  TdomXPathSyntaxNodeStack = class
  private
    FNodeList: TList;
  protected
    function getLength: integer; virtual;
  public
    constructor create; virtual;
    destructor destroy; override;
    procedure clear; virtual;
    function peek(offset: integer): TdomXPathSyntaxNode; virtual;
    function pop: TdomXPathSyntaxNode; virtual;
    function push(node: TdomXPathSyntaxNode): TdomXPathSyntaxNode; virtual;
    property length: integer read getLength;
  end;

  TdomXPathSyntaxNode = class(TCustomOwnedObject)
  protected
    FLeft: TdomXPathSyntaxNode;
    FRight: TdomXPathSyntaxNode;
    FValue: wideString;
    function getOwnerSyntaxTree: TdomXPathSyntaxTree; virtual;
  public
    constructor create(const AOwner: TdomXPathSyntaxTree;
                       const value: wideString); virtual;
    property left: TdomXPathSyntaxNode read FLeft write FLeft;
    property ownerSyntaxTree: TdomXPathSyntaxTree read getOwnerSyntaxTree;
    property right: TdomXPathSyntaxNode read FRight write FRight;
    property value: wideString read FValue;
  end;

  // Cf. XPath 1.0, prod. [2].
  TdomXPathAbsoluteLocationPath = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [4].
  TdomXPathStep = class(TdomXPathSyntaxNode)
  public
    function addStep(const step: TdomXPathStep): boolean; virtual;
    function evaluate(const oldSnapshotResult: TdomXPathNodeSetResult): TdomXPathNodeSetResult; virtual;
  end;

  // Cf. XPath 1.0, prod. [6].
  // This class is only used as a common ancestor of the axis name classes below.
  TdomXPathCustomAxisName = class(TdomXPathSyntaxNode)
  protected
    FAxisType: TdomXPathAxisType;
    FPrincipalNodeType: TdomNodeType;
    function getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult; virtual; abstract;
  public
    constructor create(const AOwner: TdomXPathSyntaxTree;
                       const value: wideString); override;
    function evaluate(const oldSnapshotResult: TdomXPathNodeSetResult): TdomXPathNodeSetResult; virtual;
    property axisType: TdomXPathAxisType read FAxisType;
  end;

  // Cf. XPath 1.0, prod. [6].
  TdomXPathAxisNameAncestor = class(TdomXPathCustomAxisName)
  protected
    function getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult; override;
  public
    constructor create(const AOwner: TdomXPathSyntaxTree;
                       const value: wideString); override;
  end;

  // Cf. XPath 1.0, prod. [6].
  TdomXPathAxisNameAncestorOrSelf = class(TdomXPathCustomAxisName)
  protected
    function getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult; override;
  public
    constructor create(const AOwner: TdomXPathSyntaxTree;
                       const value: wideString); override;
  end;

  // Cf. XPath 1.0, prod. [6].
  TdomXPathAxisNameAttribute = class(TdomXPathCustomAxisName)
  protected
    function getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult; override;
  public
    constructor create(const AOwner: TdomXPathSyntaxTree;
                       const value: wideString); override;
  end;

  // Cf. XPath 1.0, prod. [6].
  TdomXPathAxisNameChild = class(TdomXPathCustomAxisName)
  protected
    function getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult; override;
  end;

  // Cf. XPath 1.0, prod. [6].
  TdomXPathAxisNameDescendant = class(TdomXPathCustomAxisName)
  protected
    function getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult; override;
  end;

  // Cf. XPath 1.0, prod. [6].
  TdomXPathAxisNameDescendantOrSelf = class(TdomXPathCustomAxisName)
  protected
    function getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult; override;
  end;

  // Cf. XPath 1.0, prod. [6].
  TdomXPathAxisNameFollowing = class(TdomXPathCustomAxisName)
  protected
    function getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult; override;
  end;

  // Cf. XPath 1.0, prod. [6].
  TdomXPathAxisNameFollowingSibling = class(TdomXPathCustomAxisName)
  protected
    function getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult; override;
  end;

  // Cf. XPath 1.0, prod. [6].
  TdomXPathAxisNameNamespace = class(TdomXPathCustomAxisName)
  protected
    function getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult; override;
  public
    constructor create(const AOwner: TdomXPathSyntaxTree;
                       const value: wideString); override;
  end;

  // Cf. XPath 1.0, prod. [6].
  TdomXPathAxisNameParent = class(TdomXPathCustomAxisName)
  protected
    function getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult; override;
  end;

  // Cf. XPath 1.0, prod. [6].
  TdomXPathAxisNamePreceding = class(TdomXPathCustomAxisName)
  protected
    function getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult; override;
  public
    constructor create(const AOwner: TdomXPathSyntaxTree;
                       const value: wideString); override;
  end;

  // Cf. XPath 1.0, prod. [6].
  TdomXPathAxisNamePrecedingSibling = class(TdomXPathCustomAxisName)
  protected
    function getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult; override;
  public
    constructor create(const AOwner: TdomXPathSyntaxTree;
                       const value: wideString); override;
  end;

  // Cf. XPath 1.0, prod. [6].
  TdomXPathAxisNameSelf = class(TdomXPathCustomAxisName)
  protected
    function getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult; override;
  end;

  // Cf. XPath 1.0, prod. [7].
  TdomXPathNodeTest = class(TdomXPathSyntaxNode)
  public
    function evaluate(const oldsnapshotResult: TdomXPathNodeSetResult;
                      const principalNodeType: TdomNodeType): TdomXPathNodeSetResult; virtual;
  end;

  // Cf. XPath 1.0, prod. [8].
  TdomXPathPredicate = class(TdomXPathSyntaxNode)
  public
    function evaluate(const oldSnapshotResult: TdomXPathNodeSetResult): TdomXPathNodeSetResult; virtual;
  end;

  // Cf. XPath 1.0, prod. [14].
  TdomXPathExpr = class(TdomXPathSyntaxNode)
  public
    function evaluate(const contextNode: TdomNode;
                      const contextPosition: Integer;
                      const contextSize: Integer): TdomXPathCustomResult; virtual;
  end;

  // Cf. XPath 1.0, prod. [15].
  TdomXPathPrimaryExpr = class(TdomXPathSyntaxNode)
  public
    function evaluate(const contextNode: TdomNode;
                      const contextPosition: Integer;
                      const contextSize: Integer): TdomXPathCustomResult; virtual;
  end;

  // Cf. XPath 1.0, prod. [16].
  TdomXPathFunctionCall = class(TdomXPathSyntaxNode)
  private
    FArguments: TList;
  protected
    FPrefix: wideString;
    FLocalName: wideString;
    FXPathFunction: TdomXPathFunction;
    function getFunctionName: wideString; virtual;
    function lookupNamespaceURI: wideString;
    procedure setFunctionName(const aFunctionName: wideString); virtual;
  public
    constructor create(const AOwner: TdomXPathSyntaxTree;
                       const value: wideString); override;
    destructor destroy; override;
    function evaluate(const contextNode: TdomNode;
                      const contextPosition: Integer;
                      const contextSize: Integer): TdomXPathCustomResult; virtual;
    property arguments: TList read FArguments;
    property functionName: wideString read getFunctionName write setFunctionName;
  end;

  // Cf. XPath 1.0, prod. [18].
  TdomXPathUnionExpr = class(TdomXPathSyntaxNode)
  public
    function evaluate(const contextNode: TdomNode;
                      const contextPosition: Integer;
                      const contextSize: Integer): TdomXPathCustomResult; virtual;
  end;

  // Cf. XPath 1.0, prod. [19].
  TdomXPathPathExpr = class(TdomXPathSyntaxNode)
  public
    function addStep(const step: TdomXPathStep): boolean; virtual;
    function evaluate(const contextNode: TdomNode;
                      const contextPosition: Integer;
                      const contextSize: Integer): TdomXPathCustomResult; virtual;
  end;

  // Cf. XPath 1.0, prod. [20].
  TdomXPathFilterExpr = class(TdomXPathSyntaxNode)
  public
    function evaluate(const contextNode: TdomNode;
                      const contextPosition: Integer;
                      const contextSize: Integer): TdomXPathCustomResult; virtual;
  end;

  // Cf. XPath 1.0, prod. [21].
  TdomXPathOrExpr = class(TdomXPathSyntaxNode)
  public
    function evaluate(const contextNode: TdomNode;
                      const contextPosition,
                            contextSize: Integer): TdomXPathCustomResult; virtual;
  end;

  // Cf. XPath 1.0, prod. [22].
  TdomXPathAndExpr = class(TdomXPathSyntaxNode)
  public
    function evaluate(const contextNode: TdomNode;
                      const contextPosition,
                            contextSize: Integer): TdomXPathCustomResult; virtual;
  end;

  // Cf. XPath 1.0, prod. [23].
  TdomXPathEqualityExpr = class(TdomXPathSyntaxNode)
  public
    function evaluate(const contextNode: TdomNode;
                      const contextPosition,
                            contextSize: Integer): TdomXPathCustomResult; virtual; abstract;
  end;

  TdomXPathIsEqualExpr = class(TdomXPathEqualityExpr)
  public
    function evaluate(const contextNode: TdomNode;
                      const contextPosition,
                            contextSize: Integer): TdomXPathCustomResult; override;
  end;

  TdomXPathIsNotEqualExpr = class(TdomXPathEqualityExpr)
  public
    function evaluate(const contextNode: TdomNode;
                      const contextPosition,
                            contextSize: Integer): TdomXPathCustomResult; override;
  end;

  // Cf. XPath 1.0, prod. [24].
  TdomXPathRelationalExpr = class(TdomXPathSyntaxNode)
  public
    function evaluate(const contextNode: TdomNode;
                      const contextPosition,
                            contextSize: Integer): TdomXPathCustomResult; virtual; abstract;
  end;

  TdomXPathLessThanExpr = class(TdomXPathRelationalExpr)
  public
    function evaluate(const contextNode: TdomNode;
                      const contextPosition,
                            contextSize: Integer): TdomXPathCustomResult; override;
  end;

  TdomXPathLessThanOrEqualExpr = class(TdomXPathRelationalExpr)
    function evaluate(const contextNode: TdomNode;
                      const contextPosition,
                            contextSize: Integer): TdomXPathCustomResult; override;
  end;

  TdomXPathGreaterThanExpr = class(TdomXPathRelationalExpr)
  public
    function evaluate(const contextNode: TdomNode;
                      const contextPosition,
                            contextSize: Integer): TdomXPathCustomResult; override;
  end;

  TdomXPathGreaterThanOrEqualExpr = class(TdomXPathRelationalExpr)
  public
    function evaluate(const contextNode: TdomNode;
                      const contextPosition,
                            contextSize: Integer): TdomXPathCustomResult; override;
  end;

  // Cf. XPath 1.0, prod. [25].
  TdomXPathAdditiveExpr = class(TdomXPathSyntaxNode)
  public
    function evaluate(const contextNode: TdomNode;
                      const contextPosition,
                            contextSize: Integer): TdomXPathCustomResult; virtual; abstract;
  end;

  TdomXPathPlusExpr = class(TdomXPathAdditiveExpr)
  public
    function evaluate(const contextNode: TdomNode;
                      const contextPosition,
                            contextSize: Integer): TdomXPathCustomResult; override;
  end;

  TdomXPathMinusExpr = class(TdomXPathAdditiveExpr)
  public
    function evaluate(const contextNode: TdomNode;
                      const contextPosition,
                            contextSize: Integer): TdomXPathCustomResult; override;
  end;

  // Cf. XPath 1.0, prod. [26].
  TdomXPathMultiplicativeExpr = class(TdomXPathSyntaxNode)
  public
    function evaluate(const contextNode: TdomNode;
                      const contextPosition,
                            contextSize: Integer): TdomXPathCustomResult; virtual; abstract;
  end;

  TdomXPathMultiplyExpr = class(TdomXPathMultiplicativeExpr)
  public
    function evaluate(const contextNode: TdomNode;
                      const contextPosition,
                            contextSize: Integer): TdomXPathCustomResult; override;
  end;

  TdomXPathDivExpr = class(TdomXPathMultiplicativeExpr)
  public
    function evaluate(const contextNode: TdomNode;
                      const contextPosition,
                            contextSize: Integer): TdomXPathCustomResult; override;
  end;

  TdomXPathModExpr = class(TdomXPathMultiplicativeExpr)
  public
    function evaluate(const contextNode: TdomNode;
                      const contextPosition,
                            contextSize: Integer): TdomXPathCustomResult; override;
  end;

  // Cf. XPath 1.0, prod. [27].
  TdomXPathUnaryExpr = class(TdomXPathSyntaxNode)
  public
    function evaluate(const contextNode: TdomNode;
                      const contextPosition,
                            contextSize: Integer): TdomXPathCustomResult; virtual;
  end;

  // Cf. XPath 1.0, prod. [28].
  TdomXPathLeftParenthesis = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [28].
  TdomXPathRightParenthesis = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [28].
  TdomXPathLeftSquareBracket = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [28].
  TdomXPathRightSquareBracket = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [28].
  TdomXPathSingleDot = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [28].
  TdomXPathDoubleDot = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [28].
  TdomXPathCommercialAt = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [28].
  TdomXPathComma = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [28].
  TdomXPathDoubleColon = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [29].
  TdomXPathLiteral = class(TdomXPathSyntaxNode)
  public
    function evaluate: TdomXPathCustomResult; virtual;
  end;

  // Cf. XPath 1.0, prod. [30].
  TdomXPathNumber = class(TdomXPathSyntaxNode)
  public
    function evaluate: TdomXPathCustomResult; virtual;
  end;

  // Cf. XPath 1.0, prod. [32].
  TdomXPathSlashOperator = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [32].
  TdomXPathShefferStrokeOperator = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [32].
  TdomXPathPlusOperator = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [32].
  TdomXPathMinusOperator = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [32].
  TdomXPathIsEqualOperator = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [32].
  TdomXPathIsNotEqualOperator = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [32].
  TdomXPathLessThanOperator = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [32].
  TdomXPathLessThanOrEqualOperator = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [32].
  TdomXPathGreaterThanOperator = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [32].
  TdomXPathGreaterThanOrEqualOperator = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [33].
  TdomXPathAndOperator = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [33].
  TdomXPathOrOperator = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [33].
  TdomXPathModOperator = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [33].
  TdomXPathDivOperator = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [34].
  TdomXPathMultiplyOperator = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [35].
  TdomXPathFunctionName = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [36].
  TdomXPathVariableReference = class(TdomXPathSyntaxNode)
  protected
    FPrefix: wideString;
    FLocalName: wideString;
    function lookupNamespaceURI: wideString;
  public
    constructor create(const aOwner: TdomXPathSyntaxTree;
                       const value: wideString); override;
    function evaluate: TdomXPathCustomResult; virtual;
  end;

  // Cf. XPath 1.0, prod. [37].
  TdomXPathNameTest = class(TdomXPathSyntaxNode)
  protected
    FPrefix: wideString;
    FLocalName: wideString;
    function lookupNamespaceURI: wideString;
  public
    constructor create(const aOwner: TdomXPathSyntaxTree;
                       const value: wideString); override;
    function evaluate(const oldSnapshotResult: TdomXPathNodeSetResult;
                      const principalNodeType: TdomNodeType): TdomXPathNodeSetResult; virtual;
  end;

  // Cf. XPath 1.0, prod. [38].
  TdomXPathNodeTypeComment = class(TdomXPathSyntaxNode)
  public
    function evaluate(const oldSnapshotResult: TdomXPathNodeSetResult): TdomXPathNodeSetResult; virtual;
  end;

  // Cf. XPath 1.0, prod. [38].
  TdomXPathNodeTypeNode = class(TdomXPathSyntaxNode)
  end;

  // Cf. XPath 1.0, prod. [38].
  TdomXPathNodeTypePI = class(TdomXPathSyntaxNode)
  public
    function evaluate(const oldSnapshotResult: TdomXPathNodeSetResult): TdomXPathNodeSetResult; virtual;
  end;

  // Cf. XPath 1.0, prod. [38].
  TdomXPathNodeTypeText = class(TdomXPathSyntaxNode)
  public
    function evaluate(const oldSnapshotResult: TdomXPathNodeSetResult): TdomXPathNodeSetResult; virtual;
  end;

{ XPath Helper Functions}
function XPathRound(const d: double): double;
function XPathWideStringToNumber(const s: wideString): double;

{ XPath Conversion Functions }
function XPathBooleanFunc(const oldResult: TdomXPathCustomResult): TdomXPathBooleanResult;
function XPathNumberFunc(const oldResult: TdomXPathCustomResult): TdomXPathNumberResult;
function XPathStringFunc(const oldResult: TdomXPathCustomResult): TdomXPathStringResult;

{ XPath Function Library -- see XPath 1.0, sec. 4 }

{ XPath Node Set Functions -- see XPath 1.0, sec. 4.1. }

function XPathFunctionLast(const contextNode: TdomNode;
                           const contextPosition: Integer;
                           const contextSize: Integer;
                           const arguments: TList): TdomXPathCustomResult;

function XPathFunctionPosition(const contextNode: TdomNode;
                               const contextPosition: Integer;
                               const contextSize: Integer;
                               const arguments: TList): TdomXPathCustomResult;

function XPathFunctionCount(const contextNode: TdomNode;
                            const contextPosition: Integer;
                            const contextSize: Integer;
                            const arguments: TList): TdomXPathCustomResult;

function XPathFunctionId(const contextNode: TdomNode;
                         const contextPosition: Integer;
                         const contextSize: Integer;
                         const arguments: TList): TdomXPathCustomResult;

function XPathFunctionLocalName(const contextNode: TdomNode;
                                const contextPosition: Integer;
                                const contextSize: Integer;
                                const arguments: TList): TdomXPathCustomResult;

function XPathFunctionNamespaceUri(const contextNode: TdomNode;
                                   const contextPosition: Integer;
                                   const contextSize: Integer;
                                   const arguments: TList): TdomXPathCustomResult;

function XPathFunctionName(const contextNode: TdomNode;
                           const contextPosition: Integer;
                           const contextSize: Integer;
                           const arguments: TList): TdomXPathCustomResult;

{ XPath String Functions -- see XPath 1.0, sec. 4.2. }

function XPathFunctionString(const contextNode: TdomNode;
                             const contextPosition: Integer;
                             const contextSize: Integer;
                             const arguments: TList): TdomXPathCustomResult;

function XPathFunctionConcat(const contextNode: TdomNode;
                             const contextPosition: Integer;
                             const contextSize: Integer;
                             const arguments: TList): TdomXPathCustomResult;

function XPathFunctionStartsWith(const contextNode: TdomNode;
                                 const contextPosition: Integer;
                                 const contextSize: Integer;
                                 const arguments: TList): TdomXPathCustomResult;

function XPathFunctionContains(const contextNode: TdomNode;
                               const contextPosition: Integer;
                               const contextSize: Integer;
                               const arguments: TList): TdomXPathCustomResult;

function XPathFunctionSubstringBefore(const contextNode: TdomNode;
                                      const contextPosition: Integer;
                                      const contextSize: Integer;
                                      const arguments: TList): TdomXPathCustomResult;

function XPathFunctionSubstringAfter(const contextNode: TdomNode;
                                     const contextPosition: Integer;
                                     const contextSize: Integer;
                                     const arguments: TList): TdomXPathCustomResult;

function XPathFunctionSubstring(const contextNode: TdomNode;
                                const contextPosition: Integer;
                                const contextSize: Integer;
                                const arguments: TList): TdomXPathCustomResult;

function XPathFunctionStringLength(const contextNode: TdomNode;
                                   const contextPosition: Integer;
                                   const contextSize: Integer;
                                   const arguments: TList): TdomXPathCustomResult;

function XPathFunctionNormalizeSpace(const contextNode: TdomNode;
                                     const contextPosition: Integer;
                                     const contextSize: Integer;
                                     const arguments: TList): TdomXPathCustomResult;

function XPathFunctionTranslate(const contextNode: TdomNode;
                                const contextPosition: Integer;
                                const contextSize: Integer;
                                const arguments: TList): TdomXPathCustomResult;

{ XPath Boolean Functions -- see XPath 1.0, sec. 4.3. }

function XPathFunctionBoolean(const contextNode: TdomNode;
                              const contextPosition: Integer;
                              const contextSize: Integer;
                              const arguments: TList): TdomXPathCustomResult;

function XPathFunctionNot(const contextNode: TdomNode;
                          const contextPosition: Integer;
                          const contextSize: Integer;
                          const arguments: TList): TdomXPathCustomResult;

function XPathFunctionTrue(const contextNode: TdomNode;
                           const contextPosition: Integer;
                           const contextSize: Integer;
                           const arguments: TList): TdomXPathCustomResult;

function XPathFunctionFalse(const contextNode: TdomNode;
                            const contextPosition: Integer;
                            const contextSize: Integer;
                            const arguments: TList): TdomXPathCustomResult;

function XPathFunctionLang(const contextNode: TdomNode;
                           const contextPosition: Integer;
                           const contextSize: Integer;
                           const arguments: TList): TdomXPathCustomResult;

{ XPath Number Functions -- see XPath 1.0, sec. 4.4. }

function XPathFunctionNumber(const contextNode: TdomNode;
                             const contextPosition: Integer;
                             const contextSize: Integer;
                             const arguments: TList): TdomXPathCustomResult;

function XPathFunctionSum(const contextNode: TdomNode;
                          const contextPosition: Integer;
                          const contextSize: Integer;
                          const arguments: TList): TdomXPathCustomResult;

function XPathFunctionFloor(const contextNode: TdomNode;
                            const contextPosition: Integer;
                            const contextSize: Integer;
                            const arguments: TList): TdomXPathCustomResult;

function XPathFunctionCeiling(const contextNode: TdomNode;
                              const contextPosition: Integer;
                              const contextSize: Integer;
                              const arguments: TList): TdomXPathCustomResult;

function XPathFunctionRound(const contextNode: TdomNode;
                            const contextPosition: Integer;
                            const contextSize: Integer;
                            const arguments: TList): TdomXPathCustomResult;


// Whitespace Processing
function normalizeSpace(const S: wideString): wideString;
function normalizeWhiteSpace(const S: wideString): wideString;
function trimSpace(const S: wideString): wideString;
function trimWhitespace(const S: wideString): wideString;
function trimWhitespaceLeft(const S: wideString): wideString;
function trimWhitespaceRight(const S: wideString): wideString;

// Routines for XML Namespace Processing
function xmlExtractPrefix(const qualifiedName: wideString): wideString;
function xmlExtractLocalName(const qualifiedName: wideString): wideString;
function xmlExtractPrefixAndLocalName(    qualifiedName: wideString;
                                      out prefix,
                                          localName: wideString): boolean;

// Entity Reference Conversion
function escapeDelimiters(const S: wideString): wideString;

// Character Reference Conversion
function resolveCharRefs(const S: wideString): wideString;
function xmlCharRefToInt(const S: wideString): integer;
function xmlCharRefToStr(const S: wideString): wideString;
function xmlIntToCharRef(const value: longint): wideString;
function xmlIntToCharRefHex(const value: longint): wideString;

procedure xmlAnalyseEntityDef(    source: wideString;
                              var entityValue,
                                  systemLiteral,
                                  pubidLiteral,
                                  nDataName: wideString;
                              var error: boolean);

procedure xmlAnalyseNotationDecl(    decl: wideString;
                                 var systemLiteral,
                                     pubidLiteral: wideString;
                                 var error: boolean);

procedure xmlAnalyseTag(    source: wideString;
                        var tagName,
                            attribSequence: wideString);

procedure xmlIsolateQuote(    source: wideString;
                          var content,
                              rest: wideString;
                          var quoteType: WideChar;
                          var error: boolean);

function xmlReplaceQuotes(const source: wideString): wideString;

procedure xmlTruncRoundBrackets(    source: wideString;
                                var content: wideString;
                                var error: boolean);

{$ifndef IGNORE_DOCUMENT_FORMAT}
var
  domDocumentFormatList: PdomDocumentFormat = nil;
{$endif}

implementation

uses
  LangUtils, UnicodeUtils, UriUtils, XmlRulesUtils,
    // The above units are contained in the Open XML Utilities package 1.x
    // available at "http://www.philo.de/xml/".
  Math;

{$IFNDEF VER140+}
// Redeclarations of constants and functions which are not available in
// Delphi 5 or below.
const
  NaN =  0.0 / 0.0;

function IsNan(const AValue: Double): Boolean;
begin
  Result := ((PInt64(@AValue)^ and $7FF0000000000000)  = $7FF0000000000000) and
            ((PInt64(@AValue)^ and $000FFFFFFFFFFFFF) <> $0000000000000000);
end;

function IsInfinite(const AValue: Double): Boolean;
begin
  Result := ((PInt64(@AValue)^ and $7FF0000000000000) = $7FF0000000000000) and
            ((PInt64(@AValue)^ and $000FFFFFFFFFFFFF) = $0000000000000000);
end;

type
  TValueSign = -1..1;

const
  NegativeValue = Low(TValueSign);
  ZeroValue = 0;
  PositiveValue = High(TValueSign);

function Sign(const AValue: Double): TValueSign;
begin
  if ((PInt64(@AValue)^ and $7FFFFFFFFFFFFFFF) = $0000000000000000) then
    Result := ZeroValue
  else if ((PInt64(@AValue)^ and $8000000000000000) = $8000000000000000) then
    Result := NegativeValue
  else
    Result := PositiveValue;
end;

{$ENDIF}

procedure xmlAnalyseTag(    source: wideString;
                        var tagName,
                            AttribSequence: wideString);
// 'Source': The tag, to be analyzed.
// 'tagName': Returns the namen of the tag.
// 'AttribSequence': Returns the Attributes, if existing.
var
  i,j,sourceLength : integer;
begin
  sourceLength:= length(Source);  // buffer storage to increase performance

  // Evaluate tagName:
  i:= 1;
  while i <= sourceLength do begin
    if IsXmlWhiteSpace(Source[i]) then break;
    inc(i);
  end;

  tagName:= copy(Source,1,i-1);

  // Evaluate Attributes:
  while i < sourceLength do begin
    inc(i);
    if not IsXmlWhiteSpace(Source[i]) then break;
  end;
  j:= length(Source);
  while j >= i do begin
    if not IsXmlWhiteSpace(Source[j]) then break;
    dec(j);
  end;

  AttribSequence:= copy(Source,i,j-i+1);
end;

procedure xmlAnalyseEntityDef(    source: wideString;
                              var entityValue,
                                  systemLiteral,
                                  pubidLiteral,
                                  nDataName: wideString;
                              var error: boolean);
// 'source': The entity definition to be analyzed.
// 'entityValue','systemLiteral','pubidLiteral','nDataName':
//    Return the respective values, if declared.
// 'error': Returns 'true', if the entity definition is not well-formed.
const
  SQ: WideChar = #39; // Code of '
  DQ: WideChar = #34; // Code of "
var
  intro: wideString;
  notaName: wideString;
  pubidLit: wideString;  // = ''
  quoteType: WideChar;
  rest: wideString;
  S: wideString;
  Src: wideString;
  systemLit: wideString; // = ''
begin
  entityValue:= '';
  SystemLiteral:= '';
  PubidLiteral:= '';
  NDataName:= '';
  Error:= false;
  if Length(Source) < 2 then begin Error:= true; exit; end;

  Src:= trimWhitespaceLeft(Source); // Remove leading white space.
  if (Src[1] = SQ) or (Src[1] = DQ) then begin
    XMLIsolateQuote(Src,entityValue,rest,QuoteType,Error);
    if Error then exit;
    if rest <> '' then begin Error:= true; exit; end;
    if not isXmlEntityValueChars(entityValue)
      then begin Error:= true; exit; end;
  end else begin
    intro:= copy(Src,1,6);
    if (intro = 'SYSTEM') or (intro = 'PUBLIC') then begin
      S:= copy(Src,7,maxint);
      if S = '' then begin Error:= true; exit; end;
      if not IsXmlWhiteSpace(S[1]) then begin Error:= true; exit; end;

      if (intro = 'SYSTEM') then begin
        XMLIsolateQuote(S,SystemLit,S,QuoteType,Error);
        if Error then exit;
      end else begin
        XMLIsolateQuote(S,PubidLit,S,QuoteType,Error);
        if Error then exit;
        if not isXmlPubidChars(PubidLit)
          then begin Error:= true; exit; end;
        XMLIsolateQuote(S,SystemLit,S,QuoteType,Error);
        if Error then exit;
      end;

      if S <> '' then begin
        if (length(S) > 6) and (copy(S,1,5) = 'NDATA') and isXmlWhitespace(S[6]) then begin
          notaName:= trimWhitespace(copy(S,7,maxint));
          if IsXmlName(notaName)
            then NDataName:= notaName
            else begin Error:= true; exit; end;
        end else begin Error:= true; exit; end;
      end;

    end else begin Error:= true; exit; end;
    SystemLiteral:= SystemLit;
    PubidLiteral:= PubidLit;
  end; {if (S[1] ... }
end;


procedure XMLAnalyseNotationDecl(    Decl: wideString;
                                 var SystemLiteral,
                                     PubidLiteral: wideString;
                                 var Error: boolean);
// 'Decl': The notation declaration to be analyzed.
// 'SystemLiteral','PubidLiteral','NDataName':
//    Return the respective values, if declared.
// 'Error': Returns 'true', if the notation declaration is not well-formed.
var
  dummy: wideString;
  intro: wideString;
  PubidLit: wideString;   // = ''
  QuoteType: WideChar;
  Source: wideString;
  SystemLit: wideString;  // = ''
begin
  SystemLiteral:= '';
  PubidLiteral:= '';
  Error:= false;
  if Length(Decl) < 2 then begin Error:= true; exit; end;

  Source:= trimWhitespace(Decl);
  intro:= copy(Source,1,6);

  if (intro <> 'SYSTEM') and (intro <> 'PUBLIC') then begin Error:= true; exit; end;

  Dummy:= copy(Source,7,Length(Source)-6);
  Source:= dummy; // Necessary, because of Delphi's problem when copying WideStrings.
  if Source = '' then begin Error:= true; exit; end;
  if not IsXmlWhiteSpace(Source[1]) then begin Error:= true; exit; end;

  if (intro = 'SYSTEM') then begin
    XMLIsolateQuote(Source,SystemLit,dummy,QuoteType,Error);
    if Error then exit;
    if dummy <> '' then begin Error:= true; exit; end;
  end else begin
    XMLIsolateQuote(Source,PubidLit,dummy,QuoteType,Error);
    Source:= dummy;
    if Error then exit;
    if not isXmlPubidChars(PubidLit)
      then begin Error:= true; exit; end;
    if Source <> '' then begin
      if not IsXmlSystemLiteral(Source) then begin Error:= true; exit; end;
      SystemLit:= copy(Source,2,length(Source)-2);
    end;
  end;

  SystemLiteral:= SystemLit;
  PubidLiteral:= PubidLit;
end;

function xmlReplaceQuotes(const source: wideString): wideString;
// This function replaces all single and double quotes
// with their respective character references.
var
  i: integer;
  content: TUtilsCustomWideStr;
begin
  result:= '';
  content:= TUtilsCustomWideStr.create;
  try
    for i:= 1 to length(source) do begin
      case ord(source[i]) of
        39: content.addWideString('&#39;'); // Single quote
        34: content.addWideString('&#34;'); // Double quote
      else
        content.addWideChar(source[i]);
      end;
    end;
    result:= content.value;
  finally
    content.free;
  end;
end;

procedure XMLTruncRoundBrackets(    Source: wideString;
                                var content: wideString;
                                var Error: boolean);
{Die Prozedur entfernt evtl. vorhandenen White-Space am Anfang und Ende
 von 'Source', prüft dann, ob der verbleibende wideString durch runde
 KLammern -- '(' und ')' -- eingerahmt wird. Ist dies der Fall, wird vom
 Klammer-Inhalt erneut evtl. vorhandener Leerraum am Anfang und Ende
 entfernt und das Ergebnis in 'content' zurückgegeben sowie 'Error' auf
 'false' gesetzt. Ist dies nicht der Fall, gibt 'content' einen leeren
 wideString ('') zurück und 'Error' wird auf 'true' gesetzt.}
var
  BracketStr: wideString;
begin
  content:= '';
  BracketStr:= trimWhitespace(Source);
  if length(BracketStr) < 2 then begin Error:= true; exit; end;
  if (BracketStr[1] <> '(') or (BracketStr[length(BracketStr)] <> ')')
    then Error:= true
    else begin
      content:= trimWhitespace(copy(BracketStr,2,Length(BracketStr)-2));
      Error:= false;
    end;
end;

procedure xmlIsolateQuote(    Source: wideString;
                          var content,
                              rest: wideString;
                          var QuoteType: WideChar;
                          var Error: boolean);
{Analysiert einen wideString ('Source'):  Führender White-Space wird
 abgeschnitten, danach wird ein in einfache oder doppelte Anführungs-
 zeichen gesetzter Text (der auch leer sein kann) erwartet, dessen Inhalt
 in 'content' zurückgegeben wird.  Falls ein Zeichen in 'content' kein
 legales XML-Zeichen ist, wird 'Error = true' zurückgegen. 'QuoteType'
 gibt den Wert der Anführungszeichen zurück (#39; für einfache und #34;
 für doppelte Anführungszeichen).  Wird nach dem Entfernen des führenden
 White-Spaces kein Anführungszeichen gefunden oder fehlt das
 korrespondierende Schlußzeichen, wird die Routine abgebrochen und 'Error
 = true' zurückgegeben. Anschließend wird überprüft, ob direkt nach dem
 Schlußzeichen etwas anderes als White-Space folgt (bzw. der wideString
 zuende ist).  Falls etwas anderes folgt, wird 'Error = true' zurückgegeben.
 Falls nicht, wird bis zum nächsten Nicht-White-Space-Zeichen gesucht und
 der Rest des WideStrings in 'rest' zurückgegeben. Für alle Fälle, in denen
 'Error = true' zurückgegen wird, werden 'content' und 'rest' als leer
 ('') und 'QuoteType' als #0; zurückgegeben.}
const
  SQ: WideChar = #39; // code of '
  DQ: WideChar = #34; // code of "
var
  i,quotepos: integer;
  dummy: wideString;
begin
  content:= '';
  rest:= '';
  if Length(Source) < 2 then begin Error:= true; exit; end;
  Error:= false;

  dummy:= trimWhitespaceLeft(Source); // Remove leading white space.
  Source:= dummy; // Necessary, because of Delphi's problem when copying WideStrings.
  QuoteType:= Source[1];
  if (QuoteType <> SQ) and (QuoteType <> DQ)
    then begin QuoteType:= #0; Error:= true; exit; end;
  Dummy:= Copy(Source,2,Length(Source)-1);
  Source:= dummy;  // Necessary, because of Delphi's problem when copying WideStrings.
  QuotePos:= Pos(wideString(QuoteType),Source);
  if QuotePos = 0 then begin QuoteType:= #0; Error:= true; exit; end;
  if Length(Source) > QuotePos then
    if not IsXmlWhiteSpace(Source[QuotePos+1])
      then begin QuoteType:= #0; Error:= true; exit; end; // No White-Space after quotation mark
  content:= Copy(Source,1,QuotePos-1);
  if not isXmlChars(content) then begin content:= ''; QuoteType:= #0; Error:= true; exit; end;
  // Strip White-Space:
  i:= QuotePos + 1;
  while (i <= length(Source)) do begin
    if not IsXmlWhiteSpace(Source[i]) then break;
    inc(i);
  end;
  if i <= Length(Source) then rest:= copy(Source,i,Length(Source)-i+1);
end;

{ Whitespace Processing }

function normalizeSpace(const S: wideString): wideString;
const
  NULL:  WideChar = #0;   // End of wideString mark
  SPACE: WideChar = #$20;
var
  I : Integer;
  LastPCharWasSpace : Boolean;
  P : PWideChar;
begin
  SetLength(Result, Length(S));
  I := 0;

  // Skip leading spaces:
  P := PWideChar(S);
  while P^ = SPACE do
    Inc(P);

  LastPCharWasSpace := False;
  while P^ <> NULL do begin
    if P^ = SPACE then begin
      LastPCharWasSpace := True;
    end else begin
      if LastPCharWasSpace then begin
        Inc(I);
        Result[I] := SPACE;
        LastPCharWasSpace := False;
      end;
      Inc(I);
      Result[I] := P^;
    end;
    Inc(P);
  end;

  SetLength(Result, I);
end;

function normalizeWhiteSpace(const S: wideString): wideString;
const
  NULL:  WideChar = #0;   // End of wideString mark
  SPACE: WideChar = #$20;
var
  I : Integer;
  LastPCharWasWhitespace : Boolean;
  P : PWideChar;
begin
  SetLength(Result, Length(S));
  I := 0;

  // Skip leading white space:
  P := PWideChar(S);
  while IsXmlWhiteSpace(P^) do
    Inc(P);

  LastPCharWasWhitespace := False;
  while P^ <> NULL do begin
    if IsXmlWhiteSpace(P^) then begin
      LastPCharWasWhitespace := True;
    end else begin
      if LastPCharWasWhitespace then begin
        Inc(I);
        Result[I] := SPACE;
        LastPCharWasWhitespace := False;
      end;
      Inc(I);
      Result[I] := P^;
    end;
    Inc(P);
  end;

  SetLength(Result, I);
end;

function trimSpace(const S: wideString): wideString;
// This function removes all spaces (#$20) at the beginning
// or end of 'S'.
const
  SPACE: WideChar = #$20;
var
  i, l: integer;
begin
  l:= Length(S);
  i:= 1;
  while (i <= l) and (S[i] = SPACE) do inc(i);
  if i > l then
    result:= ''
  else begin
    while (S[l] = SPACE) do dec(l);
    result:= copy(S,i,l-i+1);
  end;
end;

function trimWhitespace(const S: wideString): wideString;
var
  i, l: integer;
begin
  l:= Length(S);
  i:= 1;
  while (i <= l) and IsXmlWhiteSpace(S[i]) do inc(i);
  if i > l then
    result:= ''
  else begin
    while IsXmlWhiteSpace(S[l]) do dec(l);
    result:= copy(S,i,l-i+1);
  end;
end;

function trimWhitespaceLeft(const S: wideString): wideString;
var
  i, l: integer;
begin
  l:= Length(S);
  i:= 1;
  while (i <= l) and IsXmlWhiteSpace(S[i]) do inc(i);
  result:= copy(s,i,Maxint);
end;

function trimWhitespaceRight(const S: wideString): wideString;
var
  i: integer;
begin
  i:= Length(S);
  while (i > 0) and IsXmlWhiteSpace(S[i]) do dec(I);
  result:= copy(s,1,i);
end;

function xmlExtractPrefix(const qualifiedName: wideString): wideString;
var
  colonpos: integer;
  localpart: wideString;  // = 0
  prefix: wideString;     // = 0
begin
  colonpos:= pos(':',qualifiedName);
  if colonpos = 0
    then localpart:= qualifiedName
    else begin
      prefix:= copy(qualifiedName,1,colonpos-1);
      localpart:= copy(qualifiedName,colonpos+1,length(qualifiedName)-colonpos);
      if not IsXmlPrefix(prefix)
        then raise EInvalid_Character_Err.create('Invalid character error.');
    end;
  if not IsXmlLocalPart(localpart)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  result:= prefix;
end;

function xmlExtractLocalName(const qualifiedName: wideString): wideString;
var
  colonpos: integer;
  prefix,localpart: wideString;
begin
  colonpos:= pos(':',qualifiedName);
  if colonpos = 0
    then localpart:= qualifiedName
    else begin
      prefix:= copy(qualifiedName,1,colonpos-1);
      localpart:= copy(qualifiedName,colonpos+1,length(qualifiedName)-colonpos);
      if not IsXmlPrefix(prefix)
        then raise EInvalid_Character_Err.create('Invalid character error.');
  end;
  if not IsXmlLocalPart(localpart)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  result:= localpart;
end;

function xmlExtractPrefixAndLocalName(    qualifiedName: wideString;
                                      out prefix,
                                          localName: wideString): boolean;
var
  colonpos: integer;
begin
  colonpos:= pos(':',qualifiedName);
  if colonpos = 0 then begin
      prefix:= '';
      if IsXmlLocalPart(qualifiedName) then begin
        localName:= qualifiedName;
        result:= true;
      end else begin
        localName:= '';
        result:= false;
      end;
    end else begin
      prefix:= copy(qualifiedName,1,colonpos-1);
      localName:= copy(qualifiedName,colonpos+1,length(qualifiedName)-colonpos);
      if IsXmlPrefix(prefix) and IsXmlLocalPart(localName) then begin
        result:= true;
      end else begin
        prefix:= '';
        localName:= '';
        result:= false;
      end;
  end;
end;

function escapeDelimiters(const S: wideString): wideString;
var
  content: TUtilsCustomWideStr;
  i: integer;
begin
  content:= TUtilsCustomWideStr.create;
  try
    for i:= 1 to length(S) do
      case ord(S[i]) of
        34: content.addWideString('&quot;');
        38: content.addWideString('&amp;');
        39: content.addWideString('&apos;');
        60: content.addWideString('&lt;');
        62: content.addWideString('&gt;');
      else
        content.addWideChar(S[i]);
      end;
    result:= content.value;
  finally
    content.free;
  end;
end;

function resolveCharRefs(const S: wideString): wideString;
const
  BOM: wideChar = #$FEFF;  // Byte order mark 
var
  i,j,indexpos: integer;
  SChar, SChar2: wideChar;
  ref: wideString;
  content: TUtilsCustomWideStr;
begin
  result:= '';
  content:= TUtilsCustomWideStr.create;
  try
    i:= 1;

    // Test for byte order mark:
    if length(S) > 0 then begin
      if S[1] = BOM then begin
        content.addWideChar(BOM);
        i:= 2;
      end;
    end;

    while i <= length(S) do begin
      SChar:= WideChar((PWideChar(S)+i-1)^);
      if IsUtf16LowSurrogate(sChar)
        then raise EConvertError.Create('WideString must not start with a UTF-16 low surrogate.');
      if IsUtf16HighSurrogate(SChar) then begin
        if i = length(s)
          then raise EConvertError.Create('WideString must not end with a UTF-16 high surrogate.');
        inc(i);
        content.addWideChar(SChar);
        SChar:= WideChar((PWideChar(S)+i-1)^);
        if not IsUtf16LowSurrogate(SChar)
          then raise EConvertError.Create('WideString contains an UTF-16 high surrogate without its corresponding low surrogate.');
      end;
      if not IsXmlChar(SChar)
        then raise EConvertError.Create('WideString contains an invalid character.');
      if SChar = '&' then begin {Reference?}
        indexpos:= -1;
        for j:= i+1 to length(S) do begin
          SChar2:= WideChar((PWideChar(S)+j-1)^);
          if SChar2 = ';' then begin indexpos:= j; break; end;
        end;
        if indexpos = -1
          then raise EConvertError.Create('WideString contains an ''&'' without a '';''.');
        ref:= copy(S,i,indexpos-i+1);
        if IsXmlEntityRef(ref) then begin
          content.addWideString(ref);
        end else if IsXmlCharRef(ref) then begin
          content.addWideString(XmlCharRefToStr(ref));
        end else raise EConvertError.CreateFmt('WideString contains an invalid reference %S.',[ref]);
        i:= indexpos;
      end else content.addWideChar(SChar);
      inc(i);
    end; {while ...}
    result:= content.value;
  finally
    content.free;
  end;
end;

function xmlCharRefToInt(const S: wideString): integer;
var
  value: word;
begin
  if not IsXmlCharRef(S)
    then raise EConvertError.CreateFmt('%S is not a valid XmlCharRef value.',[S]);
  if S[3] = 'x'
    then Result:= StrToInt(concat('$',copy(S,4,length(S)-4))) // Hex
    else Result:= StrToInt(copy(S,3,length(S)-3));            // Dec
  if Result > $10FFFF
    then raise EConvertError.CreateFmt('%S is not a valid XmlCharRef value.',[S]);
  if Result < $10000 then begin
    value:= Result;
    if not IsXmlChar(WideChar(value))
      then raise EConvertError.CreateFmt('%S is not a valid XmlCharRef value.',[S]);
    case result of
      $D800..$DBFF, // Reserved for high surrogate of Unicode character [$10000..$10FFFF]
      $DC00..$DFFF: // Reserved for low surrogate of Unicode character [$10000..$10FFFF]
      raise EConvertError.CreateFmt('%S is not a valid XmlCharRef value.',[S]);
    end; {case ...}
  end; {if ...}
end;

function xmlCharRefToStr(const S: wideString): wideString;
var
  value: integer;
  smallValue: word;
begin
  value:= XmlCharRefToInt(S);
  if value < $10000 then begin
    smallValue:= value;
    Result:= wideString(WideChar(smallValue));
  end else
    Result:= concat(wideString(Utf16HighSurrogate(value)),
                    wideString(Utf16LowSurrogate(value)));
end;

function xmlIntToCharRef(const value: longint): wideString;
begin
  result:= concat('&#',intToStr(value),';');
end;

function xmlIntToCharRefHex(const value: longint): wideString;
begin
  result:= concat('&#x',IntToHex(value,1),';');
end;

function XPathRound(const d: double): double;
begin
  if IsNaN(D) or IsInfinite(D)
    then Result := D
    else Result := Floor(D + 0.5);
  if Result = 0 then
    if D < 0 then
      Result := 0 / -1; // Set to negative null.    // xxx Does this work?
end;

function XPathWideStringToNumber(const s: wideString): double;
var
  DecimalPointFound: Boolean;
  E: Integer;
  B, P: PWideChar;
  W: Word;
begin
  Result := NaN;
  DecimalPointFound := False;
  E := 0;
  B := PWideChar(S);
  P := B + Length(S) - 1;

  // Skip trailing white space:
  while (P >= B) and IsXmlWhiteSpace(P^) do
    Dec(P);

  if P >= B then begin
    if ord(P^) = $2E then begin // '.' at the end of the number --> invalid
      Result := NaN;
      Exit;
    end else Result := 0;
  end;

  while P >= B do begin
    W := ord(P^);
    if W <= $39 then begin
      if W >= $30 then begin // Digit
        Result := Result + ( (W - $30) * Power(10, E) );
        Inc(E);
      end else if W = $2E then begin // '.'
        if DecimalPointFound then begin
          Result := NaN;
          Exit;
        end;
        Result := Result / Power(10, E);
        E := 0;
        DecimalPointFound := True;
      end else if W = $2D then begin // '-'
        Result := -Result;
        Dec(P);
        Break;
      end else Break;
    end else Break;
    Dec(P);
  end;

  // Skip leading white space:
  while (P >= B) and IsXmlWhiteSpace(P^) do
    Dec(P);

  if P >= B then Result := NaN;
end;

function XPathBooleanFunc(const oldResult: TdomXPathCustomResult): TdomXPathBooleanResult;
begin
  if not assigned(oldResult)
    then raise ENot_Supported_Err.create('Not supported error.');
  if oldResult.resultType = XPATH_BOOLEAN_TYPE then begin
    result:= (oldResult as TdomXPathBooleanResult);
  end else begin
    result:= TdomXPathBooleanResult.create(oldResult.asBoolean);
    oldResult.Free;
  end;
end;

function XPathNumberFunc(const oldResult: TdomXPathCustomResult): TdomXPathNumberResult;
begin
  if not assigned(oldResult)
    then raise ENot_Supported_Err.create('Not supported error.');
  if oldResult.resultType = XPATH_NUMBER_TYPE then begin
    result:= (oldResult as TdomXPathNumberResult);
  end else begin
    result:= TdomXPathNumberResult.create(oldResult.asNumber);
    oldResult.Free;
  end;
end;

function XPathStringFunc(const oldResult: TdomXPathCustomResult): TdomXPathStringResult;
begin
  if not assigned(oldResult)
    then raise ENot_Supported_Err.create('Not supported error.');
  if oldResult.resultType = XPATH_STRING_TYPE then begin
    result:= (oldResult as TdomXPathStringResult);
  end else begin
    result:= TdomXPathStringResult.create(oldResult.asWideString);
    oldResult.Free;
  end;
end;



//+++++++++++++++++++++++++ TDomBaseComponent +++++++++++++++++++++++++
function TDomBaseComponent.getXdomVersion: wideString;
begin
  result:= '3.1.14';
end;



//++++++++++++++++++++++++ TDomImplementation +++++++++++++++++++++++++
constructor TDomImplementation.create(aOwner: TComponent);
begin
  inherited create(aOwner);
{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
  FCreatedASModelsNS:= TList.create;
{$endif}
  FCreatedDocumentsListing:= TList.create;
  FCreatedDocuments:= TdomNodeList.create(FCreatedDocumentsListing);
  FCreatedDocumentTypesListing:= TList.create;
  FCreatedDocumentTypes:= TdomNodeList.create(FCreatedDocumentTypesListing);
{$ifndef IGNORE_DOCUMENT_FORMAT}
  FDefaultDocumentClass:= TdomDocument;
{$endif}
  FResourceResolver:= nil;
  FTabWidth:= 4;
  FErrorReportLevel := 0;
end;

destructor TDomImplementation.destroy;
begin
  clear;
  FCreatedDocumentsListing.free;
  FCreatedDocuments.free;
  FCreatedDocumentTypesListing.free;
  FCreatedDocumentTypes.free;
{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
  FCreatedASModelsNS.free;
{$endif}
  inherited destroy;
end;

procedure TDomImplementation.clear;
var
  i: integer;
begin
  for i:= 0 to FCreatedDocumentsListing.count-1 do begin
    TdomDocument(FCreatedDocumentsListing[i]).clear; // destroys all child nodes, nodeIterators and treeWalkers
    TdomDocument(FCreatedDocumentsListing[i]).free;
    FCreatedDocumentsListing[i]:= nil;
  end;
  FCreatedDocumentsListing.pack;
  FCreatedDocumentsListing.Capacity:= FCreatedDocumentsListing.Count;

  for i:= 0 to FCreatedDocumentTypesListing.count-1 do begin
    TdomDocumentType(FCreatedDocumentTypesListing[i]).free;
    FCreatedDocumentTypesListing[i]:= nil;
  end;
  FCreatedDocumentTypesListing.pack;
  FCreatedDocumentTypesListing.Capacity:= FCreatedDocumentTypesListing.Count;

{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
  for i:= 0 to pred(FCreatedASModelsNS.count)
    do TdomASModelNS(FCreatedASModelsNS[i]).free;
  FCreatedASModelsNS.clear;
{$endif}
end;

{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
function TDomImplementation.createASModelNS: TdomASModelNS;
begin
  Result:= TdomASModelNS.create(self);
  FCreatedASModelsNS.add(Result);
end;
{$endif}

function TDomImplementation.createDoc: TdomDocument;
begin
  result:= getDocInstance;
  FCreatedDocumentsListing.add(result);
end;

function TDomImplementation.createDocument(const name: wideString;
                                                 doctype: TdomDocumentType): TdomDocument;
begin
  if not IsXmlName(name)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  result:= getDocumentInstance(name,doctype);
  FCreatedDocumentsListing.add(result);
  result.InitDoc(name);
end;

function TDomImplementation.createDocumentNS(const namespaceURI,
                                                   qualifiedName: wideString;
                                                   doctype: TdomDocumentType): TdomDocument;
var
  prfx, localName: wideString;
begin
  if assigned(doctype) then
    if assigned(doctype.referenceDocument) or (documentTypes.IndexOf(doctype) = -1)
      then raise EWrong_Document_Err.create('Wrong document error.');
  if not xmlExtractPrefixAndLocalName(qualifiedName,prfx,localName) then begin
    if not IsXmlName(qualifiedName)
      then raise EInvalid_Character_Err.create('Invalid character error.')
      else raise ENamespace_Err.create('Namespace error.');
  end;
  if ( ((prfx = 'xmlns') or (qualifiedName = 'xmlns'))
    and not (namespaceURI = 'http://www.w3.org/2000/xmlns/') )
      then raise ENamespace_Err.create('Namespace error.');
  if (namespaceURI = '') and (prfx <> '')
    then raise ENamespace_Err.create('Namespace error.');
  if (prfx = 'xml') and (namespaceURI <> 'http://www.w3.org/XML/1998/namespace')
    then raise ENamespace_Err.create('Namespace error.');
  result:= getDocumentInstanceNS(namespaceURI,qualifiedName,doctype);
  FCreatedDocuments.FNodeList.add(Result);
  Result.InitDocNS(namespaceURI,qualifiedName);
end;

function TDomImplementation.createDocumentType(const qualifiedName,
                                                     publicId,
                                                     systemId: wideString): TdomDocumentType;
begin
  if not IsXmlName(qualifiedName)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  if not IsXmlQName(qualifiedName)
    then raise ENamespace_Err.create('Namespace error.');
  Result:= TdomDocumentType.create(self,qualifiedName,publicId,systemId);
  FCreatedDocumentTypes.FNodeList.add(Result);
end;

procedure TDomImplementation.disableErrorEvents;
begin
  inc(FErrorReportLevel);
end;

procedure TDomImplementation.enableErrorEvents;
begin
  dec(FErrorReportLevel);
end;

{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
procedure TDomImplementation.freeASModelNS(var arg: TdomASModelNS);
var
  index: integer;
  temp: TdomASModelNS;
begin
  if not assigned(arg) then exit;
  temp:= arg;
  index:= FCreatedASModelsNS.IndexOf(arg);
  if index = -1
    then raise ENot_Found_Err.create('ASModel not found error.');

  // Free the TdomASModelNS object:
  FCreatedASModelsNS.Delete(index);
  Pointer(arg):= nil;
  temp.free;
end;
{$endif}

procedure TDomImplementation.freeDocument(var doc: TdomDocument);
var
  index: integer;
  dummyDocTyp: TdomDocumentType;
begin
  if not assigned(doc) then exit;
  index:= FCreatedDocumentsListing.IndexOf(doc);
  if index = -1
    then raise ENot_Found_Err.create('Document not found error.');

  dummyDocTyp:= doc.doctype;
  // Free the document:
  FCreatedDocumentsListing.Delete(index);
  doc.free;
  doc:= nil;
  // Free doctype (we do this after freeing the document, because doc.free
  // automatically removes an associated document type node):
  if assigned(dummyDocTyp) then freeDocumentType(dummyDocTyp);
end;

procedure TDomImplementation.freeDocumentType(var docType: TdomDocumentType);
var
  index: integer;
begin
  if not assigned(docType) then exit;
  index:= FCreatedDocumentTypesListing.IndexOf(docType);
  if index = -1
    then raise ENot_Found_Err.create('DocumentType not found error.');
  If assigned(docType.ownerDocument)
    then raise EInuse_Err.create('Inuse DocumentType error.');
  FCreatedDocumentTypesListing.Delete(index);
  docType.free;
  docType:= nil;
end;

{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
procedure TDomImplementation.freeUnusedASModelsNS;
var
  i: integer;
  ASModelNS: TdomASModelNS;
begin
  for i:= pred(FCreatedASModelsNS.Count) downto 0 do begin
    ASModelNS:= TdomASModelNS(FCreatedASModelsNS.Items[i]);
    if ASModelNS.ownerCollections.Count = 0 then begin
      FCreatedASModelsNS.Delete(i);
      ASModelNS.Free;
    end;
  end;
end;
{$endif}

{$ifdef IGNORE_DOCUMENT_FORMAT}
function TDomImplementation.getDocInstance: TdomDocument;
begin
  Result:= TdomDocument.create(self,nil);
end;

function TDomImplementation.getDocumentInstance(const name: wideString;
                                                const doctype: TdomDocumentType): TdomDocument;
begin
  Result:= TdomDocument.create(self,doctype);
end;

function TDomImplementation.getDocumentInstanceNS(const aNamespaceUri,
                                                        aQualifiedName: wideString;
                                                  const doctype: TdomDocumentType): TdomDocument;
begin
  Result:= TdomDocument.create(self,doctype);
end;

{$else}

function TDomImplementation.getDocInstance: TdomDocument;
begin
  Result:= defaultDocumentClass.create(self,nil);
end;

function TDomImplementation.getDocumentInstance(const name: wideString;
                                                const doctype: TdomDocumentType): TdomDocument;
begin
  if SupportsDocumentFormat('',name)
    then Result:= getDocumentClass('',name).create(self,doctype)
    else Result:= defaultDocumentClass.create(self,doctype);
end;

function TDomImplementation.getDocumentInstanceNS(const aNamespaceUri,
                                                        aQualifiedName: wideString;
                                                  const doctype: TdomDocumentType): TdomDocument;
begin
  if SupportsDocumentFormat(aNamespaceUri,aQualifiedName)
    then Result:= getDocumentClass(aNamespaceUri,aQualifiedName).create(self,doctype)
    else Result:= defaultDocumentClass.create(self,doctype);
end;
{$endif}

function TDomImplementation.getDocuments: TdomNodeList;
begin
  Result:= FCreatedDocuments;
end;

function TDomImplementation.getDocumentTypes: TdomNodeList;
begin
  Result:= FCreatedDocumentTypes;
end;

function TDomImplementation.getErrorEventsDisabled: Boolean;
begin
  Result := FErrorReportLevel > 0;
end;

procedure TDomImplementation.doAttrModified(const modifiedNode: TdomNode;
                                            const attrChange: TdomAttrChange;
                                            const relatedAttr: TdomAttr);
begin
  if assigned(FOnAttrModified)
    then FOnAttrModified(self,modifiedNode,attrChange,relatedAttr);
end;

procedure TDomImplementation.doCharacterDataModified(modifiedNode: TdomNode);
begin
  if assigned(FOnCharacterDataModified)
    then FOnCharacterDataModified(self,modifiedNode);
end;

procedure TDomImplementation.doError(    sender: TObject;
                                         error: TdomError;
                                     var go: boolean);
begin
  case error.severity of
    DOM_SEVERITY_WARNING, DOM_SEVERITY_ERROR: go:= true;
    DOM_SEVERITY_FATAL_ERROR: go:= false;
  end;
  if assigned(FOnError) and (FErrorReportLevel = 0)
    then FOnError(sender,error,go);
end;

procedure TDomImplementation.doNodeClearing(node: TdomNode);
begin
  if assigned(FOnNodeClearing)
    then FOnNodeClearing(self,node);
end;

procedure TDomImplementation.doNodeInserted(node: TdomNode);
begin
  if assigned(FOnNodeInserted)
    then FOnNodeInserted(self,node);
end;

procedure TDomImplementation.doNodeRemoving(node: TdomNode);
begin
  if assigned(FOnNodeRemoving)
    then FOnNodeRemoving(self,node);
end;

procedure TDomImplementation.doRequestXPathFunctionResult(const namespaceUri,
                                                                localName: wideString;
                                                          const contextNode: TdomNode;
                                                          const contextPosition,
                                                                contextSize: Integer;
                                                          const arguments: TList;
                                                            var value: TdomXPathCustomResult);
begin
  if assigned(FOnRequestXPathFunctionResult)
    then FOnRequestXPathFunctionResult(namespaceUri, localName, contextNode, contextPosition, contextSize, arguments, value);
end;

procedure TDomImplementation.doRequestXPathVariable(const XPathExpression: TXPathExpression;
                                                    const namespaceURI,
                                                          localName: wideString;
                                                      var value: TdomXPathCustomResult);
begin
  if assigned(FOnRequestXPathVariable)
    then FOnRequestXPathVariable(XPathExpression, namespaceURI, localName, value);
end;

function TDomImplementation.hasFeature(const feature,
                                             version: wideString): boolean;
var
  VersionStr: string;
begin
  Result:= false;
  VersionStr:= WideCharToString(PWideChar(feature));
  if (WideCharToString(PWideChar(version))='1.0')
    or (WideCharToString(PWideChar(version))='')
  then begin
    if (CompareText(VersionStr,'XML')=0)
       then Result:= true;
  end else begin
    if (WideCharToString(PWideChar(version))='2.0')
      then begin
        if (CompareText(VersionStr,'XML')=0)
           then Result:= true;
        if (CompareText(VersionStr,'VIEWS')=0)
           then Result:= true;
        if (CompareText(VersionStr,'TRAVERSAL')=0)
           then Result:= true;
    end else begin
      if version = ''
        then begin
          if (CompareText(VersionStr,'XML')=0)
             then Result:= true;
          if (CompareText(VersionStr,'VIEWS')=0)
             then Result:= true;
          if (CompareText(VersionStr,'TRAVERSAL')=0)
             then Result:= true;
      end;  {if ... }
    end;  {if ... else ...}
  end; {if ... else ...}
end;

{$ifndef IGNORE_DOCUMENT_FORMAT}
function TDomImplementation.getDocumentClass(const aNamespaceUri,
                                                   aQualifiedName: wideString): TdomDocumentClass;
var
  aDocFormat: PdomDocumentFormat;
begin
  aDocFormat := domDocumentFormatList;
  while aDocFormat <> nil do
    with aDocFormat^ do begin
      if (aNamespaceUri = NamespaceUri) and (aQualifiedName = qualifiedName) then begin
        Result:= DocumentClass;
        exit;
      end else aDocFormat := next;
    end;
  raise EUnknown_Document_Format_Err.create('Unknown document format yet');
end;
{$endif}

function TDomImplementation.handleError(const sender: TObject;
                                        const error: TdomError): boolean;
begin
  if not assigned(error)
    then raise ENot_Supported_Err.create('Not supported error.');
  doError(sender,error,result);
end;

procedure TDomImplementation.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FResourceResolver)
    then FResourceResolver := nil;
end;

{$ifndef IGNORE_DOCUMENT_FORMAT}
class procedure TDomImplementation.RegisterDocumentFormat(const aNamespaceUri,
                                                                aQualifiedName: wideString;
                                                                aDocumentClass: TdomDocumentClass);
var
  newRec: PdomDocumentFormat;
begin
  new(newRec);
  with newRec^ do begin
    documentClass:= aDocumentClass;
    NamespaceUri:= aNamespaceUri;
    qualifiedName:= aQualifiedName;
    next:= domDocumentFormatList;
  end;
  domDocumentFormatList:= newRec;
end;
{$endif}

function TDomImplementation.resolveResource(const aBaseURI: wideString;
                                              var publicId,
                                                  systemId: wideString): TStream;
begin
  if not assigned(ResourceResolver) then
    raise ENot_Found_Err.create('No resource resolver assigned to DOM implementation.');
  Result := ResourceResolver.ResolveResource(aBaseUri, publicId, systemId);
end;

{$ifndef IGNORE_DOCUMENT_FORMAT}
procedure TDomImplementation.setDefaultDocumentClass(const value: TdomDocumentClass);
begin
  FDefaultDocumentClass:= value;
end;
{$endif}

procedure TDomImplementation.setTabWidth(const value: integer);
begin
  FTabWidth := value;
end;

procedure TDomImplementation.setResourceResolver(const AResourceResolver: TCustomResourceResolver);
begin
  if FResourceResolver = AResourceResolver then exit;
  {$IFDEF VER140+}
  if assigned(FResourceResolver)
    then FResourceResolver.RemoveFreeNotification(Self);
  {$ENDIF}
  {$IFDEF LINUX}
  if assigned(FResourceResolver)
    then FResourceResolver.RemoveFreeNotification(Self);
  {$ENDIF}
  FResourceResolver := AResourceResolver;
  if assigned(AResourceResolver)
    then AResourceResolver.FreeNotification(Self);
end;

{$ifndef IGNORE_DOCUMENT_FORMAT}
function TDomImplementation.supportsDocumentFormat(const aNamespaceUri,
                                                         aQualifiedName: wideString): boolean;
var
  aDocFormat: PdomDocumentFormat;
begin
  Result:= false;
  aDocFormat:= domDocumentFormatList;
  while aDocFormat <> nil do
    with aDocFormat^ do begin
      if (aNamespaceUri = NamespaceUri) and (aQualifiedName = qualifiedName) then begin
        Result:= true;
        exit;
      end else aDocFormat := next;
    end;
end;

class procedure TDomImplementation.UnregisterDocumentClass(const aDocumentClass: TdomDocumentClass);
var
  aDocFormat,oldRec,previous: PdomDocumentFormat;
begin
  previous:= nil;
  aDocFormat := domDocumentFormatList;
  while aDocFormat <> nil do
    with aDocFormat^ do begin
      if aDocumentClass = DocumentClass then begin
        oldRec:= aDocFormat;
        if assigned(previous)
          then previous.next:= next
          else domDocumentFormatList := next;
        previous:= aDocFormat;
        aDocFormat := next;
        Dispose(oldRec);
      end else begin
        previous:= aDocFormat;
        aDocFormat := next;
      end;
    end; {with ...}
end;
{$endif}


//++++++++++++++++++++++++++++ TdomTreeWalker +++++++++++++++++++++++++++++++
constructor TdomTreeWalker.create(const Root: TdomNode;
                                  const WhatToShow: TdomWhatToShow;
                                  const NodeFilter: TdomNodeFilter;
                                  const EntityReferenceExpansion: boolean);
begin
  if not assigned(Root)
    then raise ENot_Supported_Err.create('Not supported error.');
  inherited create;
  FWhatToShow:= WhatToShow;
  FFilter:= NodeFilter;
  FExpandEntityReferences:= EntityReferenceExpansion;
  FRoot:= Root;
  FCurrentNode:= Root;
end;

procedure TdomTreeWalker.SetCurrentNode(const node: TdomNode);
begin
  if not assigned(node)
    then raise ENot_Supported_Err.create('Not supported error.');
  FCurrentNode:= node;
end;

procedure TdomTreeWalker.setExpandEntityReferences(const value: boolean);
begin
  FExpandEntityReferences:= value;
end;

procedure TdomTreeWalker.setFilter(const value: TdomNodeFilter);
begin
  FFilter:= value;
end;

procedure TdomTreeWalker.setRoot(const node: TdomNode);
begin
  if not assigned(node)
    then raise ENot_Supported_Err.create('Not supported error.');
  FRoot:= node;
end;

procedure TdomTreeWalker.setWhatToShow(const value: TdomWhatToShow);
begin
  FWhatToShow:= value;
end;

function TdomTreeWalker.FindNextSibling(const oldNode: TdomNode): TdomNode;
var
  accept: TdomFilterResult;
  newNode: TdomNode;
begin
  Result:= nil;
  if oldNode = root then exit;
  newNode:= oldNode.NextSibling;
  if assigned(newNode) then begin
    if newNode.NodeType in FWhatToShow then begin
      if assigned(FFilter)
        then accept:= FFilter.acceptNode(newNode)
        else accept:= filter_accept;
    end else accept:= filter_skip;
    case accept of
      filter_reject:
        Result:= FindNextSibling(newNode);
      filter_skip:
        begin
          Result:= FindFirstChild(newNode);
          if not assigned(result)
            then Result:= FindNextSibling(newNode);
        end;
      filter_accept:
        Result:= newNode;
    end; {case ...}
  end else begin
    if not assigned(oldNode.parentNode)
      then begin result:= nil; exit; end; // TreeWalker.root not found!
    if oldNode.parentNode.NodeType in FWhatToShow then begin
      if assigned(FFilter)
        then accept:= FFilter.acceptNode(oldNode.parentNode)
        else accept:= filter_accept;
    end else accept:= filter_skip;
    case accept of
      filter_reject, filter_skip:
        Result:= FindNextSibling(oldNode.parentNode);
      filter_accept:
        Result:= nil;
    end; {case ...}
  end;
end;

function TdomTreeWalker.FindPreviousSibling(const OldNode: TdomNode): TdomNode;
var
  accept: TdomFilterResult;
  newNode: TdomNode;
begin
  Result:= nil;
  if OldNode = root then exit;
  newNode:= oldNode.PreviousSibling;
  if assigned(newNode) then begin
    if newNode.NodeType in FWhatToShow then begin
      if assigned(FFilter)
        then accept:= FFilter.acceptNode(newNode)
        else accept:= filter_accept;
    end else accept:= filter_skip;
    case accept of
      filter_reject:
        Result:= FindPreviousSibling(newNode);
      filter_skip:
        begin
          Result:= FindLastChild(newNode);
          if not assigned(result)
            then Result:= FindPreviousSibling(newNode);
        end;
      filter_accept:
        Result:= newNode;
    end; {case ...}
  end else begin
    if not assigned(oldNode.parentNode)
      then begin result:= nil; exit; end; // TreeWalker.root not found!
    if oldNode.parentNode.NodeType in FWhatToShow then begin
      if assigned(FFilter)
        then accept:= FFilter.acceptNode(oldNode.parentNode)
        else accept:= filter_accept;
    end else accept:= filter_skip;
    case accept of
      filter_reject, filter_skip:
        Result:= FindPreviousSibling(oldNode.parentNode);
      filter_accept:
        Result:= nil;
    end; {case ...}
  end;
end;

function TdomTreeWalker.FindParentNode(const OldNode: TdomNode): TdomNode;
var
  accept: TdomFilterResult;
begin
  Result:= nil;
  if OldNode = root then exit;
  Result:= OldNode.ParentNode;
  if not assigned(Result)
    then begin result:= nil; exit; end; // TreeWalker.root not found!
  if Result.NodeType in FWhatToShow then begin
    if assigned(FFilter)
      then accept:= FFilter.acceptNode(Result)
      else accept:= filter_accept;
  end else accept:= filter_skip;
  case accept of
    filter_reject, filter_skip:
      Result:= FindParentNode(Result);
  end;
end;

function TdomTreeWalker.FindFirstChild(const oldNode: TdomNode): TdomNode;
var
  i: integer;
  newNode: TdomNode;
  accept: TdomFilterResult;
begin
  Result:= nil;
  if (oldNode.nodeType = ntEntity_Reference_Node) and not FExpandEntityReferences
    then exit;
  for i:= 0 to pred(oldnode.childNodes.length) do begin
    newNode:= oldnode.childNodes.item(i);
    if newNode.NodeType in FWhatToShow then begin
      if assigned(FFilter)
        then accept:= FFilter.acceptNode(newNode)
        else accept:= filter_accept;
    end else accept:= filter_skip;
    case accept of
      filter_skip:
        Result:= FindFirstChild(newNode);
      filter_accept:
        Result:= newNode;
    end; {case ...}
    if assigned(result) then break;
  end; {for ...}
end;

function TdomTreeWalker.FindLastChild(const OldNode: TdomNode): TdomNode;
var
  i: integer;
  newNode: TdomNode;
  accept: TdomFilterResult;
begin
  Result:= nil;
  if (oldNode.nodeType = ntEntity_Reference_Node) and not FExpandEntityReferences
    then exit;
  for i:= pred(oldnode.childNodes.length) downto 0 do begin
    newNode:= oldnode.childNodes.item(i);
    if newNode.NodeType in FWhatToShow then begin
      if assigned(FFilter)
        then accept:= FFilter.acceptNode(newNode)
        else accept:= filter_accept;
    end else accept:= filter_skip;
    case accept of
      filter_skip:
        Result:= FindLastChild(newNode);
      filter_accept:
        Result:= newNode;
    end; {case ...}
    if assigned(result) then break;
  end; {for ...}
end;

function TdomTreeWalker.FindNextNode(OldNode: TdomNode): TdomNode;
var
  newNode: TdomNode;
begin
  Result:= FindFirstChild(oldNode);
  if OldNode = root then exit;
  if not assigned(Result)
    then Result:= FindNextSibling(oldNode);
  while not assigned(Result) do begin
    newNode:= FindParentNode(oldNode);
    if not assigned(newNode) then exit;  // No next node.
    Result:= FindNextSibling(newNode);
    oldNode:= newNode;
  end;
end;

function TdomTreeWalker.FindPreviousNode(const OldNode: TdomNode): TdomNode;
var
  newNode: TdomNode;
begin
  Result:= nil;
  if OldNode = root then exit;
  Result:= FindPreviousSibling(oldNode);
  if assigned(Result) then begin
    newNode:= FindLastChild(Result);
    if assigned(newNode) then result:= newNode;
  end else
    result:= FindParentNode(oldNode);
end;

function TdomTreeWalker.parentNode: TdomNode;
begin
  Result:= FindParentNode(FCurrentNode);
  if assigned(Result) then FCurrentNode:= Result;
end;

function TdomTreeWalker.firstChild: TdomNode;
begin
  Result:= FindFirstChild(FCurrentNode);
  if assigned(Result) then FCurrentNode:= Result;
end;

function TdomTreeWalker.lastChild: TdomNode;
begin
  Result:= FindLastChild(FCurrentNode);
  if assigned(Result) then FCurrentNode:= Result;
end;

function TdomTreeWalker.previousSibling: TdomNode;
begin
  Result:= FindPreviousSibling(FCurrentNode);
  if assigned(Result) then FCurrentNode:= Result;
end;

function TdomTreeWalker.nextSibling: TdomNode;
begin
  Result:= FindNextSibling(FCurrentNode);
  if assigned(Result) then FCurrentNode:= Result;
end;

function TdomTreeWalker.previousNode: TdomNode;
begin
  Result:= FindPreviousNode(FCurrentNode);
  if assigned(Result) then FCurrentNode:= Result;
end;

function TdomTreeWalker.nextNode: TdomNode;
begin
  Result:= FindNextNode(FCurrentNode);
  if assigned(Result) then FCurrentNode:= Result;
end;



//++++++++++++++++++++++++++++ TdomNodeIterator +++++++++++++++++++++++++++++++
constructor TdomNodeIterator.create(const Root: TdomNode;
                                    const WhatToShow: TdomWhatToShow;
                                    const nodeFilter: TdomNodeFilter;
                                    const EntityReferenceExpansion: boolean);
begin
  if not assigned(Root)
    then raise ENot_Supported_Err.create('Not supported error.');
  inherited create;
  FRoot:= root;
  FWhatToShow:= WhatToShow;
  FFilter:= NodeFilter;
  FExpandEntityReferences:= EntityReferenceExpansion;
  FReferenceNode:= Root;
  FInvalid:= false;
  FPosition:= posBefore;
end;

procedure TdomNodeIterator.handleNodeEvent(const node: TdomNode;
                                           const eventType: TdomNodeEvent);
var
  newRefNode: TdomNode;
  newPosition: TdomPosition;
  TP_Ref, TP_Root: TdomTreePosition;
begin
  if FInvalid then exit;
  case eventType of
    neClearing: begin
      TP_Ref:= FReferenceNode.compareTreePosition(node);
      if Tree_Position_Following in TP_Ref then begin
        // The Iterator's reference node is affected.
        TP_Root:= FRoot.compareTreePosition(node);
        if Tree_Position_Following in TP_Root then begin
          // The Iterator's root node is affected too,
          // so we must invalidate the Iterator:
          FReferenceNode:= nil;
          FRoot:= nil;
          FInvalid:= true;
        end else begin
          // Reposition the Iterator:
          FReferenceNode:= node;
          FPosition:= posAfter;
        end;
      end;
    end;
    neRemoving: begin
      TP_Root:= FRoot.compareTreePosition(node);
      if Tree_Position_Preceding in TP_Root then begin
        TP_Ref:= FReferenceNode.compareTreePosition(node);
        if ( (Tree_Position_Following in TP_Ref) or
             (Tree_Position_Same_Node in TP_Ref) ) then begin

          newRefNode:= nil;
          newPosition:= FPosition;
          case FPosition of
            posBefore: begin
              newRefNode:= node.NextSibling;
              if not assigned(newRefNode) then begin
                newRefNode:= FindPreviousNode(node);
                newPosition:= posAfter;
              end;
            end;
            posAfter: begin
              newRefNode:= node.NextSibling;
              if not assigned(newRefNode) then begin
                newRefNode:= FindPreviousNode(node);
                newPosition:= posBefore;
              end;
            end;
          end; {case ...}
          if assigned(newRefNode) then begin
            FReferenceNode:= newRefNode;
            FPosition:= newPosition;
          end else begin
            // The Iterator is in an invalid state, so we invalidate it
            // (usually this should not happen, but we care for it anyway):
            FReferenceNode:= nil;
            FRoot:= nil;
            FInvalid:= true;
          end;
        end;
      end;
    end;
  end; {case ...}
end;

procedure TdomNodeIterator.detach;
begin
  FReferenceNode:= nil;
  FInvalid:= true;
end;

function TdomNodeIterator.FindNextNode(OldNode: TdomNode): TdomNode;
var
  newNode: TdomNode;
begin
  with OldNode do
    if HasChildNodes
      and ( FExpandEntityReferences or (nodeType <> ntEntity_Reference_Node) )
      then result:= FirstChild
      else result:= NextSibling;
  while not assigned(Result) do begin
    newNode:= oldNode.ParentNode;
    if not assigned(newNode) then exit;  // No next node.
    Result:= newNode.NextSibling;
    oldNode:= newNode;
  end;
end;

function TdomNodeIterator.FindPreviousNode(const OldNode: TdomNode): TdomNode;
var
  newNode: TdomNode;
begin
  with OldNode do begin
    result:= PreviousSibling;
    if assigned(result) then begin
      newNode:= result;
      while assigned(newNode) do begin
        result:= newNode;
        newNode:= newNode.LastChild;
      end;
    end else result:= ParentNode;
  end;
end;

function TdomNodeIterator.NextNode: TdomNode;
var
  accept: TdomFilterResult;
  newNode: TdomNode;
begin
  newNode:= nil;
  if FInvalid
    then raise EInvalid_State_Err.create('Invalid state error.');
  case FPosition of
    posBefore: begin
      FPosition:= posAfter;
      newNode:= FReferenceNode;
    end;
    posAfter: begin
      newNode:= FindNextNode(FReferenceNode);
    end;
  end;
  repeat
    accept:= filter_accept;
    if assigned(newNode) then begin
      if newNode.NodeType in FWhatToShow then begin
        if assigned(FFilter)
          then accept:= FFilter.acceptNode(newNode);
      end else accept:= filter_skip;
      if not (accept = filter_accept)
        then newNode:= FindNextNode(newNode);
    end;
  until accept = filter_accept;
  if assigned(newNode) then
    if not (newNode.hasAsAncestor(root) or (newNode = root)) then
      if (FReferenceNode.hasAsAncestor(root) or (FReferenceNode = root)) then newNode:= nil;
  if assigned(newNode) then FReferenceNode:= newNode;
  Result:= newNode;
end;

function TdomNodeIterator.PreviousNode: TdomNode;
var
  accept: TdomFilterResult;
  newNode: TdomNode;
begin
  newNode:= nil;
  if FInvalid
    then raise EInvalid_State_Err.create('Invalid state error.');
  case FPosition of
    posBefore: begin
      newNode:= FindPreviousNode(FReferenceNode);
    end;
    posAfter: begin
      FPosition:= posBefore;
      newNode:= FReferenceNode;
    end;
  end;
  repeat
    accept:= filter_accept;
    if assigned(newNode) then begin
      if newNode.NodeType in FWhatToShow then begin
        if assigned(FFilter)
          then accept:= FFilter.acceptNode(newNode);
      end else accept:= filter_skip;
      if not (accept = filter_accept)
        then newNode:= FindPreviousNode(newNode);
    end;
  until accept = filter_accept;
  if assigned(newNode) then
    if not (newNode.hasAsAncestor(root) or (newNode = root)) then
      if (FReferenceNode.hasAsAncestor(root) or (FReferenceNode = root)) then newNode:= nil;
  if assigned(newNode) then FReferenceNode:= newNode;
  Result:= newNode;
end;



//++++++++++++++++++++++++++++ TdomNodeList +++++++++++++++++++++++++++++++
constructor TdomNodeList.create(const nodeList: TList);
begin
  inherited create;
  FNodeList:= nodeList;
end;

function TdomNodeList.getLength: integer;
begin
  Result:= FNodeList.count;
end;

function TdomNodeList.indexOf(const node: TdomNode): integer;
begin
  Result:= FNodeList.IndexOf(node);
end;

function TdomNodeList.item(const index: integer): TdomNode;
begin
  if (index < 0) or (index >= FNodeList.Count)
    then result:= nil
    else result:= TdomNode(FNodeList.List^[Index]);
end;



//++++++++++++++++++++++++ TdomElementsNodeList ++++++++++++++++++++++++++
constructor TdomElementsNodeList.create(const QueryName: wideString;
                                        const StartElement: TdomNode);
begin
  inherited create(nil);
  FQueryName:= QueryName;
  FStartElement:= StartElement;
end;

function TdomElementsNodeList.GetLength: integer;
var
  AktNode,NewNode: TdomNode;
  Level: integer;
begin
  Result:= 0;
  if not assigned(FStartElement) then exit;
  Level:= 0;
  AktNode:= FStartElement;
  if AktNode.NodeType = ntElement_Node then
    if (AktNode.NodeName = FQueryName) or (FQueryName = '*') then
      inc(Result);
  repeat
    if AktNode.HasChildNodes
      then begin NewNode:= AktNode.FirstChild; inc(Level); end
      else NewNode:= AktNode.NextSibling;
    while not assigned(NewNode) do begin
      dec(Level);
      if Level < 1 then break;
      AktNode:= AktNode.ParentNode;
      NewNode:= AktNode.NextSibling;
    end;
    if Level < 1 then break;
    AktNode:= NewNode;
    if AktNode.NodeType = ntElement_Node then
      if (AktNode.NodeName = FQueryName) or (FQueryName = '*') then
        inc(Result);
  until Level < 1;
end;

function TdomElementsNodeList.IndexOf(const node: TdomNode): integer;
var
  AktNode,NewNode: TdomNode;
  Level,i: integer;
begin
  Result:= -1;
  if not assigned(FStartElement) then exit;
  if not (node is TdomNode) then exit;
  if node.NodeType <> ntElement_Node then exit;
  i:= -1;
  Level:= 0;
  AktNode:= FStartElement;
  repeat
    if AktNode.HasChildNodes
      then begin NewNode:= AktNode.FirstChild; inc(Level); end
      else NewNode:= AktNode.NextSibling;
    while not assigned(NewNode) do begin
      dec(Level);
      if Level < 1 then break;
      AktNode:= AktNode.ParentNode;
      NewNode:= AktNode.NextSibling;
    end;
    if Level < 1 then break;
    AktNode:= NewNode;
    if AktNode.NodeType = ntElement_Node then
      if (AktNode.NodeName = FQueryName) or (FQueryName = '*') then begin
        inc(i);
        if AktNode = node then begin Result:= i; break; end;
      end;
  until Level < 1;
end;

function TdomElementsNodeList.Item(const index: integer): TdomNode;
var
  AktNode,NewNode: TdomNode;
  Level,i: integer;
begin
  Result:= nil;
  if not assigned(FStartElement) then exit;
  if (index < 0) then exit;
  i:= -1;
  Level:= 0;
  AktNode:= FStartElement;
  repeat
    if AktNode.HasChildNodes
      then begin NewNode:= AktNode.FirstChild; inc(Level); end
      else NewNode:= AktNode.NextSibling;
    while not assigned(NewNode) do begin
      dec(Level);
      if Level < 1 then break;
      AktNode:= AktNode.ParentNode;
      NewNode:= AktNode.NextSibling;
    end;
    if Level < 1 then break;
    AktNode:= NewNode;
    if AktNode.NodeType = ntElement_Node then
      if (AktNode.NodeName = FQueryName) or (FQueryName = '*') then begin
        inc(i);
        if i = index then begin Result:= AktNode; break; end;
      end;
  until Level < 1;
end;



//+++++++++++++++++++++TdomElementsNodeListNS ++++++++++++++++++++++++++
constructor TdomElementsNodeListNS.create(const QueryNamespaceURI,
                                                QueryLocalName: wideString;
                                          const StartElement: TdomNode);
begin
  inherited create(nil);
  FQueryNamespaceURI:= QueryNamespaceURI;
  FQueryLocalName:= QueryLocalName;
  FStartElement:= StartElement;
end;

function TdomElementsNodeListNS.GetLength: integer;
var
  AktNode,NewNode: TdomNode;
  Level: integer;
begin
  Result:= 0;
  if not assigned(FStartElement) then exit;
  Level:= 0;
  AktNode:= FStartElement;
  repeat
    if AktNode.HasChildNodes
      then begin NewNode:= AktNode.FirstChild; inc(Level); end
      else NewNode:= AktNode.NextSibling;
    while not assigned(NewNode) do begin
      dec(Level);
      if Level < 1 then break;
      AktNode:= AktNode.ParentNode;
      NewNode:= AktNode.NextSibling;
    end;
    if Level < 1 then break;
    AktNode:= NewNode;
    if AktNode.NodeType = ntElement_Node then
      if ((AktNode.namespaceURI = FQueryNamespaceURI) or (FQueryNamespaceURI = '*'))
        and ((AktNode.localName = FQueryLocalName) or (FQueryLocalName = '*'))
          then inc(Result);
  until Level < 1;
end;

function TdomElementsNodeListNS.IndexOf(const node: TdomNode): integer;
var
  AktNode,NewNode: TdomNode;
  Level,i: integer;
begin
  Result:= -1;
  if not assigned(FStartElement) then exit;
  if not (node is TdomNode) then exit;
  if node.NodeType <> ntElement_Node then exit;
  i:= -1;
  Level:= 0;
  AktNode:= FStartElement;
  repeat
    if AktNode.HasChildNodes
      then begin NewNode:= AktNode.FirstChild; inc(Level); end
      else NewNode:= AktNode.NextSibling;
    while not assigned(NewNode) do begin
      dec(Level);
      if Level < 1 then break;
      AktNode:= AktNode.ParentNode;
      NewNode:= AktNode.NextSibling;
    end;
    if Level < 1 then break;
    AktNode:= NewNode;
    if AktNode.NodeType = ntElement_Node then
      if ((AktNode.namespaceURI = FQueryNamespaceURI) or (FQueryNamespaceURI = '*'))
        and ((AktNode.localName = FQueryLocalName) or (FQueryLocalName = '*'))
          then begin
            inc(i);
            if AktNode = node then begin Result:= i; break; end;
          end;
  until Level < 1;
end;

function TdomElementsNodeListNS.Item(const index: integer): TdomNode;
var
  AktNode,NewNode: TdomNode;
  Level,i: integer;
begin
  Result:= nil;
  if not assigned(FStartElement) then exit;
  if (index < 0) then exit;
  i:= -1;
  Level:= 0;
  AktNode:= FStartElement;
  repeat
    if AktNode.HasChildNodes
      then begin NewNode:= AktNode.FirstChild; inc(Level); end
      else NewNode:= AktNode.NextSibling;
    while not assigned(NewNode) do begin
      dec(Level);
      if Level < 1 then break;
      AktNode:= AktNode.ParentNode;
      NewNode:= AktNode.NextSibling;
    end;
    if Level < 1 then break;
    AktNode:= NewNode;
    if AktNode.NodeType = ntElement_Node then
      if ((AktNode.namespaceURI = FQueryNamespaceURI) or (FQueryNamespaceURI = '*'))
        and ((AktNode.localName = FQueryLocalName) or (FQueryLocalName = '*'))
          then begin
            inc(i);
            if i = index then begin Result:= AktNode; break; end;
          end;
  until Level < 1;
end;



//+++++++++++++++++++++++ TdomOwnerNamedNodeMap +++++++++++++++++++++++++++
constructor TdomOwnerNamedNodeMap.create(const aItemClass: TdomCustomNodeClass);
begin
  FItemClass:= aItemClass;
  FNodeList:= TUtilsWideStringList.Create;
  FNodeList.duplicates:= dupError;
  FNodeList.sorted:= true;
end;

destructor TdomOwnerNamedNodeMap.destroy;
begin
  clear;
  FNodeList.Free;
  inherited;
end;

function TdomOwnerNamedNodeMap.add(const node: TdomCustomNode): integer;
begin
  if not (node is itemClass)
    then raise EHierarchy_Request_Err.create('Hierarchy request error.');
  result:= FNodeList.addObject(node.nodeName,node);
end;

procedure TdomOwnerNamedNodeMap.clear;
var
  index: integer;
begin
  for index:= 0 to pred(FNodeList.count) do
    FNodeList.objects[index].Free;
  FNodeList.clear;
end;

procedure TdomOwnerNamedNodeMap.Delete(const index: integer);
begin
  if (index < 0) or (index >= count)
    then raise EStringListError.CreateFmt('List index out of bounds (%d)', [Index]);
  TdomCustomNode(FNodeList.objects[index]).Free;
  FNodeList.Delete(index);
end;

function TdomOwnerNamedNodeMap.extractItem(const node: TdomCustomNode): TdomCustomNode;
var
  index: integer;
begin
  index:= indexOfItem(node);
  if index = -1
    then raise EStringListError.Create('Item not found');
  result:= items[index];
  FNodeList.Delete(index);
end;

function TdomOwnerNamedNodeMap.getCount: integer;
begin
  result:= FNodeList.count;
end;

function TdomOwnerNamedNodeMap.getItems(index: integer): TdomCustomNode;
begin
  result:= TdomCustomNode(FNodeList.objects[index]);
end;

function TdomOwnerNamedNodeMap.getNamedItem(const name: wideString): TdomCustomNode;
var
  index: integer;
begin
  index:= indexOfNamedItem(name);
  if index = -1
    then result:= nil
    else result:= items[index];
end;

function TdomOwnerNamedNodeMap.hasNamedItem(const name: wideString): boolean;
begin
  result:= indexOfNamedItem(name) > -1;
end;

function TdomOwnerNamedNodeMap.indexOfItem(const node: TdomCustomNode): integer;
var
  index: integer;
begin
  for index:= 0 to pred(FNodeList.count) do
    if FNodeList.objects[index] = node then begin
      result:= index;
      exit;
    end;
  result:= -1;
end;

function TdomOwnerNamedNodeMap.indexOfNamedItem(const name: wideString): integer;
begin
  result:= FNodeList.indexOf(name);
end;

function TdomOwnerNamedNodeMap.removeItem(const node: TdomCustomNode): integer;
begin
  result:= indexOfItem(node);
  if result > -1 then begin
    FNodeList.Delete(result);
    node.Free;
  end;
end;

function TdomOwnerNamedNodeMap.removeNamedItem(const name: wideString): integer;
begin
  result:= indexOfNamedItem(name);
  if result > -1 then begin
    TdomCustomNode(FNodeList.objects[result]).Free;
    FNodeList.Delete(result);
  end;
end;




//+++++++++++++++++++++++++ TdomNamedNodeMap +++++++++++++++++++++++++++++
constructor TdomNamedNodeMap.create(const aOwner: TdomNode;
                                    const nodeList: TList;
                                    const allowedNTs: TDomNodeTypeSet;
                                    const defaultNamespaceAware: boolean);
begin
  inherited create(nodeList);
  FOwnerNode:= aOwner;
  FAllowedNodeTypes:= allowedNTs;
  FDefaultNamespaceAware:= defaultNamespaceAware;
end;

procedure TdomNamedNodeMap.checkAllowedNodeType(const node: TdomNode);
begin
  if not (node.NodeType in FAllowedNodeTypes)
    then raise EHierarchy_Request_Err.create('Hierarchy request error.');
end;

procedure TdomNamedNodeMap.checkHasNode(const node: TdomNode);
begin
  if FNodeList.indexOf(node) = -1
    then raise ENot_Found_Err.create('Node not found error.');
end;

procedure TdomNamedNodeMap.checkNamespaceAware;
begin
  if not namespaceAware
    then raise ENamespace_Err.create('Namespace error.');
end;

procedure TdomNamedNodeMap.checkNotInUse(const node: TdomNode);
begin
  if assigned(node.parentNode)
    then raise EInuse_Err.create('Inuse node error.');
  if node.NodeType = ntAttribute_Node
    then if assigned((node as TdomAttr).FOwnerMap)
      then if (node as TdomAttr).FOwnerMap <> self
        then raise EInuse_Err.create('Inuse attribute error.');
end;

procedure TdomNamedNodeMap.checkNotNamespaceAware;
begin
  if namespaceAware
    then raise ENamespace_Err.create('Namespace error.');
end;

procedure TdomNamedNodeMap.checkNotReadOnly;
begin
  if ReadOnly
    then raise ENo_Modification_Allowed_Err.create('No modification allowed error.');
end;

procedure TdomNamedNodeMap.checkSameReferenceDocument(const node: TdomNode);
begin
  if ownerNode.referenceDocument <> node.referenceDocument
    then raise EWrong_Document_Err.create('Wrong document error.');
end;

function TdomNamedNodeMap.getNamedItem(const name: wideString): TdomNode;
var
  i: integer;
begin
  checkNotNamespaceAware;
  result:= nil;
  for i:= 0 to pred(FNodeList.count) do
    if TdomNode(FNodeList[i]).NodeName = name then begin
      Result:= TdomNode(FNodeList[i]);
      break;
    end;
end;

function TdomNamedNodeMap.getNamedItemNS(const namespaceURI,
                                               localName: wideString): TdomNode;
var
  i: integer;
begin
  checkNamespaceAware;
  result:= nil;
  for i:= 0 to pred(FNodeList.count) do
    if (TdomNode(FNodeList[i]).namespaceURI = namespaceURI)
      and (TdomNode(FNodeList[i]).localName = localName) then begin
      Result:= TdomNode(FNodeList[i]);
      break;
    end;
end;

function TdomNamedNodeMap.getNamespaceAware: boolean;
begin
  if assigned(ownerNode)
    then result:= ownerNode.isNamespaceNode
    else result:= FDefaultNamespaceAware;
end;

function TdomNamedNodeMap.getReadOnly: boolean;
begin
  if assigned(ownerNode)
    then result:= ownerNode.isReadonly
    else result:= false;
end;

procedure TdomNamedNodeMap.internalAdd(const node: TdomNode);
begin
  FNodeList.Add(node);
  if (node.NodeType = ntAttribute_Node)
    then (node as TdomAttr).FOwnerMap:= self;
end;

procedure TdomNamedNodeMap.internalRemove(const node: TdomNode);
begin
  FNodeList.remove(node);
  if (node.NodeType = ntAttribute_Node)
    then (node as TdomAttr).FOwnerMap:= nil;
end;

function TdomNamedNodeMap.removeItem(const arg: TdomNode): TdomNode;
begin
  checkNotReadOnly;
  checkHasNode(arg);
  result:= arg;
  internalRemove(arg);
end;

function TdomNamedNodeMap.removeNamedItem(const name: wideString): TdomNode;
begin
  checkNotNamespaceAware;
  checkNotReadOnly;
  Result:= getNamedItem(name);
  if not assigned(Result)
    then raise ENot_Found_Err.create('Node not found error.');
  internalRemove(result);
end;

function TdomNamedNodeMap.removeNamedItemNS(const namespaceURI,
                                                  localName: wideString): TdomNode;
begin
  checkNamespaceAware;
  checkNotReadOnly;
  Result:= getNamedItemNS(namespaceURI,localName);
  if not assigned(Result)
    then raise ENot_Found_Err.create('Node not found error.');
  internalRemove(result);
end;

function TdomNamedNodeMap.setNamedItem(const arg: TdomNode): TdomNode;
begin
  checkNotNamespaceAware;
  checkNotReadOnly;
  checkSameReferenceDocument(arg);
  checkAllowedNodeType(arg);
  checkNotInUse(arg);

  result:= getNamedItem(arg.NodeName);
  if result = arg then begin  // Is arg already in the map?
    result := nil;
  end else begin
    if assigned(result)
      then internalRemove(result);
    internalAdd(arg);
  end;
end;

function TdomNamedNodeMap.setNamedItemNS(const arg: TdomNode): TdomNode;
begin
  checkNamespaceAware;
  checkNotReadOnly;
  checkSameReferenceDocument(arg);
  checkAllowedNodeType(arg);
  checkNotInUse(arg);

  result:= getNamedItemNS(arg.namespaceURI,arg.localName);
  if result = arg then begin  // Is arg already in the map?
    result := nil;
  end else begin
    if assigned(result)
      then internalRemove(result);
    internalAdd(arg);
  end;
end;



//+++++++++++++++++++++++++++ TdomCustomNode ++++++++++++++++++++++++++++++
procedure TdomCustomNode.raiseException(const E: ExceptClass);
begin
  if E = EHierarchyRequestError
    then raise EHierarchy_Request_Err.Create('EHierarchy request error.')
  else if E = ENoModificationAllowedError
    then raise ENo_Modification_Allowed_Err.Create('No modification allowed.')
  else if E = ENotAssignedError
    then raise ENot_Supported_Err.Create('Node not specified.')
  else if E = ENotFoundError
    then raise ENot_Found_Err.Create('Node not found.')
  else if E = EWrongOwnerError
    then raise EWrong_Document_Err.Create('Wrong document.')
  else raise E.Create(E.ClassName);
end;



//++++++++++++++++++++++++++++++ TdomNode +++++++++++++++++++++++++++++++++
constructor TdomNode.create(const aOwner: TCustomOwnedObject);
begin
  inherited create(aOwner);
  FNodeList:= TdomNodeList.create(items);
  FAllowedChildTypes:= [ntElement_Node,
                        ntText_Node,
                        ntCDATA_Section_Node,
                        ntEntity_Reference_Node,
                        ntProcessing_Instruction_Node,
                        ntComment_Node,
                        ntDocument_Type_Decl_Node,
                        ntDocument_Fragment_Node,
                        ntNotation_Node];
  FIsNamespaceNode:= false;
  if AOwner is TdomDocument
    then FOwnerDocument:= aOwner as TdomDocument
    else FOwnerDocument:= nil;
end;

destructor TdomNode.destroy;
var
  I: Integer;
  UserDataEvent: TdomUserDataEvent;
begin
  // Call user data event handlers:
  if Assigned(FUserData) then
    with FUserData do
      for I := 0 to Pred(Count) do begin
        @UserDataEvent := Pointer(FUserDataHandlers[I]);
        if Assigned(UserDataEvent) then
          UserDataEvent(OT_NODE_IMPORTED, WideStrings[I], Objects[I], nil, nil);
      end;

  FNodeList.Free;
  FUserData.Free;
  FUserDataHandlers.Free;
  inherited Destroy;
end;

function TdomNode.appendChild(const newChild: TdomNode): TdomNode;
begin
  checkTypeAllowed(newChild);

  if newChild is TdomDocumentFragment then begin

    checkAssigned(newChild);
    checkSameOwner(newChild);
    while newChild.hasChildNodes do
      append(newChild);
    result := newChild;

  end else result := (append(newChild) as TdomNode);
end;

procedure TdomNode.checkTypeAllowed(const node: TdomNode);
begin
  if assigned(node) then
    if not (node.NodeType in FAllowedChildTypes) then
      raise EHierarchy_Request_Err.CreateFmt(
        'Nodes of type %s are not allowed as children of nodes of type %s',
        [node.ClassName, ClassName]
      );
end;

procedure TdomNode.clear;
begin
  inherited clear;
end;

function TdomNode.cloneNode(const deep: boolean): TdomNode;
var
  I: Integer;
  UserDataEvent: TdomUserDataEvent;
begin
  Result := ReferenceDocument.ImportNode2(Self, Deep);

  // Call user data event handlers:
  if Assigned(Result) and Assigned(FUserData) then
    with FUserData do
      for I := 0 to Pred(Count) do begin
        @UserDataEvent := Pointer(FUserDataHandlers[I]);
        if Assigned(UserDataEvent) then
          UserDataEvent(OT_NODE_CLONED, WideStrings[I], Objects[I], Self, Result);
      end;
end;

function TdomNode.compareTreePosition(const other: TdomNode): TdomTreePosition;

  procedure buildAncestorList(node: TdomNode;
                              const ancestors: TList);
  begin
    ancestors.clear;
    while true do begin
      ancestors.insert(0,node);
      if assigned(node.parentNode) then begin
        node:= node.parentNode;
      end else begin
        if node.nodeType = ntAttribute_Node then begin
          if assigned(TdomAttr(node).ownerElement)
            then node:= TdomAttr(node).ownerElement
            else break;
        end else break;
      end; {if ... else ...}
    end; {while ...}
  end;

var
  selfAncestors, otherAncestors: TList;
  i: integer;
begin
  if not assigned(other)
    then raise ENot_Supported_Err.create('Not supported error.');
  if other = self then begin
    result:= [Tree_Position_Equivalent,Tree_Position_Same_Node];
    exit;
  end;

  selfAncestors:= TList.create;
  otherAncestors:= TList.create;
  try
    buildAncestorList(self,selfAncestors);
    buildAncestorList(other,otherAncestors);

    // Disconnected?
    if selfAncestors[0] <> otherAncestors[0] then begin
      result:= [Tree_Position_Disconnected];
      exit;
    end;

    // Reduce list to the last common ancestor:
    selfAncestors.Add(nil);   // Add stop-nil
    otherAncestors.Add(nil);  // Add stop-nil
    while selfAncestors[1] = otherAncestors[1] do begin
      selfAncestors.Delete(0);
      otherAncestors.Delete(0);
      // Remark: No run over, because 'self' and 'other' are not identical.
    end;

    // Is 'other' ancestor?
    if otherAncestors.count = 2 then begin // Remark: 2, because 'other' and nil are in the list.
      result:= [Tree_Position_Ancestor,Tree_Position_Preceding];
      exit;
    end;

    // Is 'other' descendant?
    if selfAncestors.count = 2 then begin
      result:= [Tree_Position_Descendant,Tree_Position_Following];
      exit;
    end;

    // Attributes involved?
    if (TdomNode(selfAncestors[1]).nodeType = ntAttribute_Node) then begin
      if (TdomNode(otherAncestors[1]).nodeType = ntAttribute_Node)
        then result:= [Tree_Position_Equivalent]
        else result:= [Tree_Position_Following];
      exit;
    end;
    if (TdomNode(otherAncestors[1]).nodeType = ntAttribute_Node) then begin
      result:= [Tree_Position_Preceding];
      exit;
    end;

    // No Attributes.  Determine the order of the nodes.
    with TdomNode(selfAncestors[0]).childNodes do begin
      for i:= 0 to pred(length) do begin
        if item(i) = selfAncestors[1] then begin
          result:= [Tree_Position_Following];
          exit;
        end;
        if item(i) = otherAncestors[1] then begin
          result:= [Tree_Position_Preceding];
          exit;
        end;
      end;
    end; {with ...}

  finally
    selfAncestors.free;
    otherAncestors.free;
  end;
end;

procedure TdomNode.doAfterAddition(const node: TCustomOwnedNode);
begin
  if assigned(referenceDocument)
    then referenceDocument.doNodeInserted(node as TdomNode);
end;

procedure TdomNode.doBeforeClear;
begin
  if assigned(referenceDocument)
    then referenceDocument.doNodeClearing(self);
end;

procedure TdomNode.doBeforeRemoval(const node: TCustomOwnedNode);
begin
  if assigned(referenceDocument)
    then referenceDocument.doNodeRemoving(node as TdomNode);
end;

function TdomNode.evaluateToBoolean(const expression: wideString): boolean;
var
  XPathExpression: TXPathExpression;
begin
  XPathExpression:= TXPathExpression.create(nil);
  try
    XPathExpression.expression:= expression;
    XPathExpression.contextNode := self;
    XPathExpression.evaluate;
    result:= XPathExpression.resultAsBoolean;
  finally
    XPathExpression.Free;
  end;
end;

function TdomNode.evaluateToNode(const expression: wideString): TdomNode;
var
  XPathExpression: TXPathExpression;
begin
  XPathExpression:= TXPathExpression.create(nil);
  try
    XPathExpression.expression:= expression;
    XPathExpression.contextNode := self;
    XPathExpression.evaluate;
    result:= XPathExpression.resultNode(0); // Remark: Returns nil, if there exists no resultNode(0).
  finally
    XPathExpression.Free;
  end;
end;

function TdomNode.evaluateToNumber(const expression: wideString): double;
var
  XPathExpression: TXPathExpression;
begin
  XPathExpression:= TXPathExpression.create(nil);
  try
    XPathExpression.expression:= expression;
    XPathExpression.contextNode := self;
    XPathExpression.evaluate;
    result:= XPathExpression.resultAsNumber;
  finally
    XPathExpression.Free;
  end;
end;

function TdomNode.evaluateToWideString(const expression: wideString): wideString;
var
  XPathExpression: TXPathExpression;
begin
  XPathExpression:= TXPathExpression.create(nil);
  try
    XPathExpression.expression:= expression;
    XPathExpression.contextNode := self;
    XPathExpression.evaluate;
    result:= XPathExpression.resultAsWideString;
  finally
    XPathExpression.Free;
  end;
end;

function TdomNode.findFirstChildElement: TdomElement;
var
  nodeToTest: TdomNode;
begin
  result:= nil;
  nodeToTest:= firstChild;
  while assigned(nodeToTest) do begin
    if nodeToTest.nodeType = ntElement_Node then begin
      result:= (nodeToTest as TdomElement);
      exit;
    end;
    nodeToTest:= nodeToTest.nextSibling;
  end;
end;

function TdomNode.findLastChildElement: TdomElement;
var
  nodeToTest: TdomNode;
begin
  result:= nil;
  nodeToTest:= lastChild;
  while assigned(nodeToTest) do begin
    if nodeToTest.nodeType = ntElement_Node then begin
      result:= (nodeToTest as TdomElement);
      exit;
    end;
    nodeToTest:= nodeToTest.previousSibling;
  end;
end;

function TdomNode.findNextSiblingElement: TdomElement;
var
  nodeToTest: TdomNode;
begin
  result:= nil;
  nodeToTest:= nextSibling;
  while assigned(nodeToTest) do begin
    if nodeToTest.nodeType = ntElement_Node then begin
      result:= (nodeToTest as TdomElement);
      exit;
    end;
    nodeToTest:= nodeToTest.nextSibling;
  end;
end;

function TdomNode.findParentElement: TdomElement;
var
  nodeToTest: TdomNode;
begin
  result:= nil;
  nodeToTest:= parentNode;
  while assigned(nodeToTest) do begin
    if nodeToTest.nodeType = ntElement_Node then begin
      result:= (nodeToTest as TdomElement);
      exit;
    end;
    nodeToTest:= nodeToTest.parentNode;
  end;
end;

function TdomNode.findPreviousSiblingElement: TdomElement;
var
  nodeToTest: TdomNode;
begin
  result:= nil;
  nodeToTest:= previousSibling;
  while assigned(nodeToTest) do begin
    if nodeToTest.nodeType = ntElement_Node then begin
      result:= (nodeToTest as TdomElement);
      exit;
    end;
    nodeToTest:= nodeToTest.previousSibling;
  end;
end;

function TdomNode.getAbsoluteIndex: integer;
var
  N: TdomNode;
begin
  N := PreviousNode;
  if Assigned(N) then begin
    Result := N.AbsoluteIndex;
    if Result > -1 then Inc(Result);
  end else Result := -1;
end;

function TdomNode.getAttributes: TdomNamedNodeMap;
begin
  Result:= nil;
end;

function TdomNode.getBaseUri: wideString;
var
  attr: TdomAttr;
  UriAnalyzer: TUriWideStrAnalyzer;
  uri1,uri2: wideString;
begin
  case nodeType of
  ntElement_Node: begin
    if isNamespaceNode
      then attr:= TdomElement(self).getAttributeNodeNS('http://www.w3.org/XML/1998/namespace','base')
      else attr:= TdomElement(self).getAttributeNode('xml:base');
    if assigned(attr) then begin

      uri1:= attr.value;
      UriAnalyzer:= TUriWideStrAnalyzer.create;
      try
        UriAnalyzer.setUriReference(uri1);
        if UriAnalyzer.HasUriScheme then begin
          // absolute URI --> we are done
          result:= attr.value;
        end else begin
          uri2:= attr.baseUri;
          ResolveRelativeUriWideStr(uri2,uri1,result);
        end;
      finally
        UriAnalyzer.free;
      end;

    end else begin
      if assigned(parentNode)
        then result:= parentNode.baseUri
        else result:= '';
    end; {if ... else ...}
  end;
  ntText_Node,ntCDATA_Section_Node,ntEntity_Reference_Node,
  ntProcessing_Instruction_Node,ntComment_Node,ntDocument_Type_Node,
  ntDocument_Type_Decl_Node:
    if assigned(parentNode)
      then result:= parentNode.baseUri
      else result:= '';
  ntAttribute_Node: begin
    result:= '';
    if assigned(TdomAttr(self).ownerElement) then begin
      if ( (namespaceURI = 'http://www.w3.org/XML/1998/namespace') and ( localName = 'base') )
         or ( (namespaceURI = '') and ( nodeName = 'xml:base') ) then begin
        if assigned(TdomAttr(self).ownerElement.parentNode)
          then result:= TdomAttr(self).ownerElement.parentNode.baseUri;
      end else result:= TdomAttr(self).ownerElement.baseUri;
    end;
  end;
  ntEntity_Node,ntNotation_Node:
    result:= referenceDocument.baseUri;
  else
    result:= '';
  end;
end;

function TdomNode.getChildNodes: TdomNodeList;
begin
  Result:= FNodeList;
end;

function TdomNode.getDocument: TdomDocument;
begin
  Result:= FOwnerDocument;
end;

function TdomNode.getExpandedName: wideString;
begin
  Result:= '';
end;

function TdomNode.getFirstChild: TdomNode;
begin
  Result := (inherited getFirstChild as TdomNode);
end;

function TdomNode.getFirstChildElement(const name: wideString): TdomElement;
var
  nodeToTest: TdomNode;
begin
  result:= nil;
  nodeToTest:= firstChild;
  while assigned(nodeToTest) do begin
    if (nodeToTest.nodeType = ntElement_Node) and (nodeToTest.nodeName = name) then begin
      result:= (nodeToTest as TdomElement);
      exit;
    end;
    nodeToTest:= nodeToTest.nextSibling;
  end;
end;

function TdomNode.getFirstChildElementNS(const namespaceURI,
                                               localName: wideString): TdomElement;
var
  nodeToTest: TdomNode;
begin
  result:= nil;
  nodeToTest:= firstChild;
  while assigned(nodeToTest) do begin
    if (nodeToTest.nodeType = ntElement_Node)
      and (nodeToTest.namespaceURI = namespaceURI)
      and (nodeToTest.localName = localName)
      then begin
      result:= (nodeToTest as TdomElement);
      exit;
    end;
    nodeToTest:= nodeToTest.nextSibling;
  end;
end;

function TdomNode.getLanguage: wideString;
var
  attr: TdomAttr;
begin
  case nodeType of
  ntElement_Node: begin
    if isNamespaceNode
      then attr:= TdomElement(self).getAttributeNodeNS('http://www.w3.org/XML/1998/namespace','lang')
      else attr:= TdomElement(self).getAttributeNode('xml:lang');
    if assigned(attr) then begin
      result:= attr.value;
    end else begin
      if assigned(parentNode)
        then result:= parentNode.language
        else result:= '';
    end; {if ... else ...}
  end;
  ntText_Node, ntCDATA_Section_Node, ntEntity_Reference_Node,
  ntProcessing_Instruction_Node, ntComment_Node:
    if assigned(parentNode)
      then result:= parentNode.language
      else result:= '';
  ntAttribute_Node:
    if assigned(TdomAttr(self).ownerElement)
      then result:= TdomAttr(self).ownerElement.language
      else result:= '';
  ntXPath_Namespace_Node:
    if assigned(TdomXPathNamespace(self).ownerElement)
      then result:= TdomXPathNamespace(self).ownerElement.language
      else result:= '';
  else
    result:= '';
  end;
end;

function TdomNode.getLastChild: TdomNode;
begin
  Result := (inherited getLastChild as TdomNode);
end;

function TdomNode.getLastChildElement(const name: wideString): TdomElement;
var
  nodeToTest: TdomNode;
begin
  result:= nil;
  nodeToTest:= lastChild;
  while assigned(nodeToTest) do begin
    if (nodeToTest.nodeType = ntElement_Node) and (nodeToTest.nodeName = name) then begin
      result:= (nodeToTest as TdomElement);
      exit;
    end;
    nodeToTest:= nodeToTest.previousSibling;
  end;
end;

function TdomNode.getLastChildElementNS(const namespaceURI,
                                              localName: wideString): TdomElement;
var
  nodeToTest: TdomNode;
begin
  result:= nil;
  nodeToTest:= lastChild;
  while assigned(nodeToTest) do begin
    if (nodeToTest.nodeType = ntElement_Node)
      and (nodeToTest.namespaceURI = namespaceURI)
      and (nodeToTest.localName = localName)
      then begin
      result:= (nodeToTest as TdomElement);
      exit;
    end;
    nodeToTest:= nodeToTest.previousSibling;
  end;
end;

function TdomNode.getLevel: integer;
begin
  if Assigned(parentNode) then begin
    Result := parentNode.level;
    if Result > -1 then Inc(Result);
  end else Result := -1;
end;

function TdomNode.getLocalName: wideString;
begin
  Result:= '';
end;

function TdomNode.getNamespaceURI: wideString;
begin
  Result:= '';
end;

function TdomNode.getNextSibling: TdomNode;
begin
  Result := (inherited getNextSibling as TdomNode);
end;

function TdomNode.getNextSiblingElement(const name: wideString): TdomElement;
var
  nodeToTest: TdomNode;
begin
  result:= nil;
  nodeToTest:= nextSibling;
  while assigned(nodeToTest) do begin
    if (nodeToTest.nodeType = ntElement_Node) and (nodeToTest.nodeName = name) then begin
      result:= (nodeToTest as TdomElement);
      exit;
    end;
    nodeToTest:= nodeToTest.nextSibling;
  end;
end;

function TdomNode.getNextSiblingElementNS(const namespaceURI,
                                                localName: wideString): TdomElement;
var
  nodeToTest: TdomNode;
begin
  result:= nil;
  nodeToTest:= nextSibling;
  while assigned(nodeToTest) do begin
    if (nodeToTest.nodeType = ntElement_Node)
      and (nodeToTest.namespaceURI = namespaceURI)
      and (nodeToTest.localName = localName)
      then begin
      result:= (nodeToTest as TdomElement);
      exit;
    end;
    nodeToTest:= nodeToTest.nextSibling;
  end;
end;

function TdomNode.getNodeName: wideString;
begin
  Result:= '';
end;

function TdomNode.getNodeType: TdomNodeType;
begin
  Result:= ntUnknown;
end;

function TdomNode.getNodeValue: wideString;
begin
  Result:= FNodeValue;
end;

function TdomNode.getParentElement(const name: wideString): TdomElement;
var
  nodeToTest: TdomNode;
begin
  result:= nil;
  nodeToTest:= parentNode;
  while assigned(nodeToTest) do begin
    if (nodeToTest.nodeType = ntElement_Node) and (nodeToTest.nodeName = name) then begin
      result:= (nodeToTest as TdomElement);
      exit;
    end;
    nodeToTest:= nodeToTest.parentNode;
  end;
end;

function TdomNode.getParentElementNS(const namespaceURI,
                                           localName: wideString): TdomElement;
var
  nodeToTest: TdomNode;
begin
  result:= nil;
  nodeToTest:= parentNode;
  while assigned(nodeToTest) do begin
    if (nodeToTest.nodeType = ntElement_Node)
      and (nodeToTest.namespaceURI = namespaceURI)
      and (nodeToTest.localName = localName)
      then begin
      result:= (nodeToTest as TdomElement);
      exit;
    end;
    nodeToTest:= nodeToTest.parentNode;
  end;
end;

function TdomNode.getParentNode: TdomNode;
begin
  Result := (inherited getParent as TdomNode);
end;

function TdomNode.getPrefix: wideString;
begin
  result:= '';
end;

function TdomNode.getPreviousSibling: TdomNode;
begin
  Result := (inherited getPreviousSibling as TdomNode);
end;

function TdomNode.getPreviousSiblingElement(const name: wideString): TdomElement;
var
  nodeToTest: TdomNode;
begin
  result:= nil;
  nodeToTest:= previousSibling;
  while assigned(nodeToTest) do begin
    if (nodeToTest.nodeType = ntElement_Node) and (nodeToTest.nodeName = name) then begin
      result:= (nodeToTest as TdomElement);
      exit;
    end;
    nodeToTest:= nodeToTest.previousSibling;
  end;
end;

function TdomNode.getPreviousSiblingElementNS(const namespaceURI,
                                                    localName: wideString): TdomElement;
var
  nodeToTest: TdomNode;
begin
  result:= nil;
  nodeToTest:= previousSibling;
  while assigned(nodeToTest) do begin
    if (nodeToTest.nodeType = ntElement_Node)
      and (nodeToTest.namespaceURI = namespaceURI)
      and (nodeToTest.localName = localName)
      then begin
      result:= (nodeToTest as TdomElement);
      exit;
    end;
    nodeToTest:= nodeToTest.previousSibling;
  end;
end;

function TdomNode.getUserData(const key: wideString): TObject;
var
  Index: Integer;
begin
  if Assigned(FUserData) then begin
    with FUserData do
      if Find(Key, Index)
        then Result := Objects[Index]
        else Result := nil;
  end else Result := nil;
end;

function TdomNode.getReferenceDocument: TdomDocument;
begin
  Result:= FOwnerDocument;
end;

function TdomNode.getTabWidth: integer;
var
  DomImpl: TDomImplementation;
begin
  if assigned(ReferenceDocument) then begin
    DomImpl := ReferenceDocument.DomImplementation;
    if assigned(DomImpl) 
      then Result := DomImpl.TabWidth
      else Result := 1;
  end else Result := 1;
end;

function TdomNode.getTextContent: wideString;
var
  childType: TdomNodeType;
  childItem: TdomNode;
  i,cl: integer;
  s: TUtilsCustomWideStr;
begin
  case nodeType of
    ntElement_Node,ntEntity_Reference_Node,ntEntity_Node,ntDocument_Fragment_Node: begin
      s := TUtilsCustomWideStr.Create;
      try
        cl:= pred(childnodes.length);
        for i:= 0 to cl do begin
          childItem:= childnodes.item(i);
          childType:= childItem.nodeType;
          if (childType <> ntComment_Node) and (childType <> ntProcessing_Instruction_Node) then
            s.AddWideString(childItem.textContent);
        end;
        result := s.Value;
      finally
        s.Free;
      end;
    end;
    ntAttribute_Node,ntText_Node,ntCDATA_Section_Node,ntComment_Node,ntProcessing_Instruction_Node:
      result:= nodeValue;
  else
    result:= '';
  end;
end;

function TdomNode.getXPathStringValue: wideString;
begin
  case nodeType of
    ntElement_Node:
      result:= textContent;
    ntAttribute_Node, ntComment_Node, ntProcessing_Instruction_Node, ntText_Node:
      result:= nodeValue;
    ntDocument_Node:
      if assigned(TdomDocument(self).documentElement)
        then result:= TdomDocument(self).documentElement.textContent
        else result:= '';
    ntXPath_Namespace_Node:
      result:= namespaceUri;
  else
    result:= '';
  end;
end;

function TdomNode.hasAsAncestor(const node: TdomNode): boolean;
begin
  result := inherited hasAsAncestor(node);
end;

function TdomNode.hasAttributes: boolean;
begin
  result:= attributes.length > 0
end;

function TdomNode.hasChildNodes: boolean;
begin
  result := HasChildren;
end;

function TdomNode.hasEntRef(const EntName: wideString): boolean;
var
  i: integer;
begin
  result:= false;
  for i:= 0 to pred(childnodes.length) do
    with childnodes.item(i) do
      if (nodeType = ntEntity_Reference_Node)
          and (nodeName = EntName)
        then result:= true
        else if HasEntRef(EntName) then begin result:= true; exit; end;
end;

function TdomNode.insertBefore(const newChild,
                                     refChild: TdomNode): TdomNode;
begin
  checkTypeAllowed(newChild);

  if newChild is TdomDocumentFragment then begin

    checkAssigned(newChild);
    checkSameOwner(newChild);
    checkDissimilarity(newChild,refChild);  // xxx remove this?
    while newChild.hasChildNodes do
      insertBefore(newChild.childNodes.item(0),refChild);
    result := newChild;

  end else result := (inherited insertBefore(newChild,refChild) as TdomNode);
end;

function TdomNode.lookupNamespaceURI(const aPrefix: wideString): wideString;
begin
  if APrefix = 'xml'
    then Result := 'http://www.w3.org/XML/1998/namespace'
    else if APrefix = 'xmlns'
      then Result := 'http://www.w3.org/2000/xmlns/'
      else Result := '';
end;

procedure TdomNode.makeChildrenReadonly;
var
  i: integer;
begin
  with childnodes do
    for i:= 0 to pred(length) do
      with item(i) do begin
        setReadOnly(true);
        makeChildrenReadonly;
      end;
end;

procedure TdomNode.normalize;
var
  i: integer;
begin
  with childNodes do
    for i:= 0 to pred(length) do
      item(i).normalize;
end;

function TdomNode.previousNode: TdomNode;
// Finds the previous node in document order.
var
  newNode: TdomNode;
begin
  result:= previousSibling;
  if assigned(result) then begin
    newNode:= result;
    while assigned(newNode) do begin
      result:= newNode;
      newNode:= newNode.lastChild;
    end;
  end else result:= parentNode;
end;

function TdomNode.removeChild(const oldChild: TdomNode): TdomNode;
begin
  Result := (inherited remove(oldChild) as TdomNode);
end;

function TdomNode.replaceChild(const newChild,
                                     oldChild: TdomNode): TdomNode;
var
  lastFragmentChild: TdomNode;
begin
  checkTypeAllowed(newChild);
  if newChild is TdomDocumentFragment then begin

    checkAssigned(newChild);
    checkSameOwner(newChild);
    checkHasChild(oldChild);

    lastFragmentChild := newChild.lastChild;
    if assigned(lastFragmentChild) then begin
      result := replaceChild(lastFragmentChild, oldChild);
      while newChild.hasChildNodes do
        insertBefore(newChild.childNodes.item(0),lastFragmentChild);
    end else result := removeChild(oldChild);

  end else result := (inherited replace(newChild, oldChild) as TdomNode);
end;

function TdomNode.resolveEntityReferences(const opt: TdomEntityResolveOption): integer;
begin
  // By default do nothing.
  result:= 0;
end;

function TdomNode.sendErrorNotification(const xmlErrorType: TXmlErrorType;
                                        const relNode: TdomNode): boolean;
// Used to centralize code for sending error notifications to the DomImplementation.
// Usually used during validation.
var
  domImpl: TDomImplementation;
  error: TdomError;
  uri: wideString;
begin
  if assigned(referenceDocument) then begin 
    domImpl:= referenceDocument.domImplementation; 
    uri:= referenceDocument.documentUri;
  end else domImpl:= nil;

  error:= TdomError.Create(XmlErrorType, -1, -1, -1, -1, -1, -1, -1, -1, uri, nil, relNode, '');
  try
    if assigned(domImpl) then begin
      result:= domImpl.handleError(domImpl,error);
    end else if error.severity = DOM_SEVERITY_FATAL_ERROR
      then result:= false
      else result:= true;
  finally
    error.free;
  end;
end;

procedure TdomNode.setNodeValue(const value: wideString);
begin
  if isReadonly then
    raise ENo_Modification_Allowed_Err.create('No modification allowed error.');
  FNodeValue:= value;
end;

procedure TdomNode.setPrefix(const value: wideString);
begin
  if isReadonly then
    raise ENo_Modification_Allowed_Err.create('No modification allowed error.');
end;

function TdomNode.setUserData(const key: wideString;
                              const data: TObject;
                              const handler: TdomUserDataEvent): TObject;
var
  Index: Integer;
begin
  if Assigned(Data) then begin

    if Assigned(FUserData) then begin
      with FUserData do
        if Find(Key, Index) then begin
          Result := Objects[Index];
          WideStrings[Index] := Key;
          Objects[Index] := Data;
          FUserDataHandlers[Index] := @Handler;
        end else begin
          Result := nil;
          AddObject(Key, Data);
          FUserDataHandlers.Add(@Handler);
        end;
    end else begin
      FUserData := TUtilsWideStringList.Create;
      FUserDataHandlers := TList.Create;
      Result := nil;
      FUserData.AddObject(Key, Data);
      FUserDataHandlers.Add(@Handler);
    end;

  end else begin

    if Assigned(FUserData) then begin
      with FUserData do
        if Find(Key, Index) then begin
          Result := Objects[Index];
          Delete(Index);
          FUserDataHandlers.Delete(Index);
          if Count = 0 then begin
            FUserData.Free;
            FUserData := nil;
            FUserDataHandlers.Free;
            FUserDataHandlers := nil;
          end;
        end else 
          Result := nil;
    end else
      Result := nil;

  end;
end;

function TdomNode.supports(const feature,
                                 version: wideString): boolean;
var
  VersionStr: string;
begin
  Result:= false;
  VersionStr:= WideCharToString(PWideChar(feature));
  if (WideCharToString(PWideChar(version))='1.0')
    or (WideCharToString(PWideChar(version))='')
  then begin
    if (CompareText(VersionStr,'XML')=0)
       then Result:= true;
  end else begin
    if (WideCharToString(PWideChar(version))='2.0')
      then begin
        if (CompareText(VersionStr,'XML')=0)
           then Result:= true;
    end; {if ...}
  end; {if ... else ...}
end;

function TdomNode.validateIDREFS: boolean;
begin
  Result := True;
end;

function TdomNode.validate2: boolean;
var
  Error: TXmlErrorType;
begin
  if not Assigned(ReferenceDocument) then
    raise EWrong_Document_Err.create('Wrong document error.');

  Error := ReferenceDocument.ValidateNode(Self);
  if Error = ET_NONE then begin
    Result := True;
  end else begin
    Result := False;
    SendErrorNotification(Error, Self);
  end;
end;



//+++++++++++++++++++++++++ TdomCharacterData ++++++++++++++++++++++++++++
constructor TdomCharacterData.create(const aOwner: TdomDocument);
begin
  inherited create(aOwner);
  FAllowedChildTypes:= [];
end;

procedure TdomCharacterData.appendData(const arg: wideString);
begin
  if isReadonly
    then raise ENo_Modification_Allowed_Err.create('No modification allowed error.');
  Data:= concat(Data,arg);
end;

procedure TdomCharacterData.deleteData(const offset,
                                             count: integer);
begin
  ReplaceData(offset,count,'');
end;

procedure TdomCharacterData.doCharacterDataModified;
begin
  if assigned(referenceDocument)
    then referenceDocument.doCharacterDataModified(self);
end;

function TdomCharacterData.getData: wideString;
begin
  Result:= NodeValue;
end;

function TdomCharacterData.getLength: integer;
begin
  Result:= System.Length(Data);
end;

procedure TdomCharacterData.insertData(const offset: integer;
                                       const arg: wideString);
begin
  ReplaceData(offset,0,arg);
end;

procedure TdomCharacterData.replaceData(const offset,
                                              count: integer;
                                        const arg: wideString);
var
  len: integer;
  Data1,Data2:wideString;
begin
  if isReadonly
    then raise ENo_Modification_Allowed_Err.create('No modification allowed error.');
  if (offset < 0) or (offset > Length) or (count < 0)
    then raise EIndex_Size_Err.create('Index size error.');
  // Make sure, that the length of the wideString is not
  // exeeded, when using count and offset:
  len:= Length-Offset;
  if count < len then len:= count;
  Data1:= SubstringData(0,offset);
  Data2:= SubstringData(offset+len,Length-offset-len);
  Data:= concat(Data1,arg,Data2);
end;

procedure TdomCharacterData.setData(const value: wideString);
var
  prevValue: wideString;
begin
  prevValue:= NodeValue;
  NodeValue:= value;
  doCharacterDataModified;
end;

function TdomCharacterData.substringData(const offset,
                                               count: integer): wideString;
var
  len: integer;
begin
  if (offset < 0) or (offset > Length) or (count < 0)
    then raise EIndex_Size_Err.create('Index size error.');
  // Make sure, that the length of the wideString is not
  // exeeded, when using count and offset:
  len:= Length-Offset;
  if count < len then len:= count;
  setString(Result,PWideChar(Data)+Offset,len);
end;



// +++++++++++++++++++++++++++++ TdomAttr +++++++++++++++++++++++++++++
constructor TdomAttr.create(const aOwner: TdomDocument;
                            const name: wideString;
                            const spcfd: boolean);
begin
  if not IsXmlName(name)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  inherited create(aOwner);
  if isXmlDefaultAttName(name) then begin
    FIsXmlnsDecl:= NSDT_DEFAULT;
  end else if isXmlPrefixedAttName(name) then begin
    FIsXmlnsDecl:= NSDT_PREFIXED;
  end else FIsXmlnsDecl:= NSDT_NONE;
  FNodeName:= name;
  FNodeValue:= '';
  FPrefix:= '';
  FSpecified:= spcfd;
  FAllowedChildTypes:= [];
end;

constructor TdomAttr.createNS(const aOwner: TdomDocument;
                              const namespaceURI,
                                    qualifiedName: wideString;
                              const spcfd: boolean);
var
  locName,prfx: wideString;
begin
  if not xmlExtractPrefixAndLocalName(qualifiedName,prfx,locName) then begin
    if not IsXmlName(qualifiedName)
      then raise EInvalid_Character_Err.create('Invalid character error.')
      else raise ENamespace_Err.create('Namespace error.');
  end;
  if prfx = 'xmlns' then begin
    if not (namespaceURI ='http://www.w3.org/2000/xmlns/')
      then raise ENamespace_Err.create('Namespace error.');
    FIsXmlnsDecl:= NSDT_PREFIXED;
  end else if qualifiedName = 'xmlns' then begin
    if not (namespaceURI ='http://www.w3.org/2000/xmlns/')
      then raise ENamespace_Err.create('Namespace error.');
    FIsXmlnsDecl:= NSDT_DEFAULT;
  end else FIsXmlnsDecl:= NSDT_NONE;
  if (namespaceURI = '') and (prfx <> '')
    then raise ENamespace_Err.create('Namespace error.');
  if (prfx = 'xml') and (namespaceURI <> 'http://www.w3.org/XML/1998/namespace')
    then raise ENamespace_Err.create('Namespace error.');
  inherited create(aOwner);
  FNodeName:= qualifiedName;
  FNamespaceURI:= namespaceURI;
  FPrefix:= prfx;
  FLocalName:= locName;
  FIsNamespaceNode:= true;
  FNodeValue:= '';
  FSpecified:= Spcfd;
  FAllowedChildTypes:= [];
end;

destructor TdomAttr.Destroy;
var
  oldReadOnly: Boolean;
begin
  if assigned(ownerElement) then begin
    with ownerElement do begin
      oldReadOnly := isReadOnly;
      setReadOnly(false);
      try
        removeAttributeNode(self)
      finally
        setReadOnly(oldReadonly);
      end;
    end;
  end;
  inherited;
end;

function TdomAttr.getExpandedName: wideString;
begin
  result:= NodeName;
end;

function TdomAttr.getIsId: boolean;
begin
  result:= false;
  if assigned(referenceDocument) then
    if assigned(OwnerElement) then
      result := referenceDocument.getAttrType(OwnerElement.NodeName, Nodename) = AS_ID_DATATYPE;
end;

function TdomAttr.getIsXmlnsDecl: TdomXmlnsDeclType;
begin
  Result := FIsXmlnsDecl;
end;

function TdomAttr.getLocalName: wideString;
begin
  Result := FLocalName;
end;

function TdomAttr.getName: wideString;
begin
  Result := NodeName;
end;

function TdomAttr.getNamespaceURI: wideString;
begin
  Result := FNamespaceURI;
end;

function TdomAttr.getNextSibling: TdomNode;
begin
  Result := nil;
end;

function TdomAttr.getNodeName: wideString;
begin
  Result := FNodeName;
end;

function TdomAttr.getNodeType: TdomNodeType;
begin
  Result := ntAttribute_Node;
end;

function TdomAttr.getOwnerElement: TdomElement;
var
  Node: TdomNode;
begin
  if Assigned(FOwnerMap) then begin
    Node := FOwnerMap.OwnerNode;
    if Node.NodeType = ntElement_Node
      then Result := (Node as TdomElement)
      else Result := nil;
  end else Result := nil;
end;

function TdomAttr.getPrefix: wideString;
begin
  Result := FPrefix;
end;

function TdomAttr.getPreviousSibling: TdomNode;
begin
  Result := nil;
end;

function TdomAttr.getSpecified: boolean;
begin
  Result := FSpecified;
end;

function TdomAttr.getValue: wideString;
begin
  Result := ReferenceDocument.CalculateNormalizedAttrValue(self);
end;

function TdomAttr.lookupNamespaceURI(const aPrefix: wideString): wideString;
begin
  if Assigned(OwnerElement)
    then Result := OwnerElement.LookupNamespaceURI(APrefix)
    else Result := inherited LookupNamespaceURI(APrefix);
end;

procedure TdomAttr.doAttrModified(const attrChange: TdomAttrChange);
begin
  if assigned(ownerElement)
    then ownerElement.doAttrModified(ownerElement,attrChange,self);
end;

procedure TdomAttr.setNodeValue(const value: wideString);
begin
  inherited;
   doAttrModified(AC_MODIFICATION);
end;

procedure TdomAttr.setPrefix(const value: wideString);
begin
  if not isXmlName(value)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  if isReadonly
    then raise ENo_Modification_Allowed_Err.create('No modification allowed error.');
  if not isXmlPrefix(value)
    then raise ENamespace_Err.create('Namespace error.');
  if namespaceURI = ''
    then raise ENamespace_Err.create('Namespace error.');
  if (value = 'xml') and (namespaceURI <> 'http://www.w3.org/XML/1998/namespace')
    then raise ENamespace_Err.create('Namespace error.');
  if (value = 'xmlns')
    and not (namespaceURI ='http://www.w3.org/2000/xmlns/')
      then raise ENamespace_Err.create('Namespace error.');
  if NodeName = 'xmlns'
    then raise ENamespace_Err.create('Namespace error.');
  FPrefix:= value;
  FNodeName:= concat(value,':',localName);
end;

function TdomAttr.validateIDREFS: boolean;
  function isValidIDREF(const idrefValue: wideString): boolean;
  var
    dummyIndex: integer;
  begin
    if not referenceDocument.IDs.find(idrefValue,dummyIndex) then begin
      result:= false;
      sendErrorNotification(ET_TARGET_ID_VALUE_NOT_FOUND,self);
    end else result:= true;
  end;

const
  SPACE: WideChar  = #$20;
var
  i, startIndex, indexCount: integer;
  attrivalue: wideString;
  TypeMismatch: boolean;
begin
  result:= true;

  try
    attriValue:= Value;
  except
    // VC: Entity declared (XML 1.0, § 4.1)
    result:= false;
    sendErrorNotification(ET_ENTITY_DECL_NOT_FOUND,self);
    exit; // Necessary because 'attriValue' would otherwise be used again in the next tests.
  end;

  // VC: IDREF (XML 1.0, § 3.3.1)
  TypeMismatch:= false;
  if assigned(OwnerElement) then begin
    case referenceDocument.GetAttrType(OwnerElement.nodeName,nodename) of

      AS_IDREF_DATATYPE:
      begin
        if isXMLName(attriValue) then begin
          if not isValidIDREF(attriValue)
            then result:= false;
        end else TypeMismatch:= true;
      end;

      AS_IDREFS_DATATYPE:
      begin
        if isXMLNames(attriValue) then begin
          startIndex:= 1; indexCount:= 0;
          for i:= 1 to length(attriValue) do begin
            if attriValue[i] = SPACE then begin
              if not isValidIDREF(copy(attriValue,startIndex,IndexCount))
                then result:= false;
              startIndex:= succ(i);
              indexCount:= 0;
            end else inc(indexCount);
          end;
          if not isValidIDREF(copy(attriValue,startIndex,IndexCount))
            then result:= false;
        end else TypeMismatch:= true;
      end;

    end; {case ...}
  end; {if ...}

  if TypeMismatch then begin
    result:= false;
    sendErrorNotification(ET_ATTRIBUTE_TYPE_MISMATCH,self);
  end;

end;



//++++++++++++++++++++++++++++ TdomElement ++++++++++++++++++++++++++++++++
constructor TdomElement.create(const aOwner: TdomDocument;
                               const tagName: wideString);
begin
  if not IsXmlName(tagName)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  inherited create(aOwner);
  FNodeName:= tagName;
  FNodeValue:= '';
  FPrefix:= '';
  FAttributeListing:= TList.create;
  FCreatedElementsNodeLists:= TList.create;
  FCreatedElementsNodeListNSs:= TList.create;
  FAttributeList:= TdomNamedNodeMap.create(self,FAttributeListing,[ntAttribute_Node],false);
  FAllowedChildTypes:= [ntElement_Node,
                        ntText_Node,
                        ntCDATA_Section_Node,
                        ntEntity_Reference_Node,
                        ntProcessing_Instruction_Node,
                        ntComment_Node,
                        ntDocument_Fragment_Node];
end;

constructor TdomElement.createNS(const aOwner: TdomDocument;
                                 const namespaceURI,
                                       qualifiedName: wideString);
var
  locName,prfx: wideString;
begin
  if not xmlExtractPrefixAndLocalName(qualifiedName,prfx,locName) then begin
    if not IsXmlName(qualifiedName)
      then raise EInvalid_Character_Err.create('Invalid character error.')
      else raise ENamespace_Err.create('Namespace error.');
  end;
  if (namespaceURI = '') and (prfx <> '')
    then raise ENamespace_Err.create('Namespace error.');
  if (prfx = 'xml') and (namespaceURI <> 'http://www.w3.org/XML/1998/namespace')
    then raise ENamespace_Err.create('Namespace error.');
  inherited create(aOwner);
  FNodeName:= qualifiedName;
  FNamespaceURI:= namespaceURI;
  FPrefix:= prfx;
  FLocalName:= locName;
  FIsNamespaceNode:= true;
  FNodeValue:= '';
  FAttributeListing:= TList.create;
  FCreatedElementsNodeLists:= TList.create;
  FCreatedElementsNodeListNSs:= TList.create;
  FAttributeList:= TdomNamedNodeMap.create(self,FAttributeListing,[ntAttribute_Node],true);
  FAllowedChildTypes:= [ntElement_Node,
                        ntText_Node,
                        ntCDATA_Section_Node,
                        ntEntity_Reference_Node,
                        ntProcessing_Instruction_Node,
                        ntComment_Node,
                        ntDocument_Fragment_Node];
end;

destructor TdomElement.destroy;
var
  i: integer;
begin
  readOnly:= false;
  doBeforeClear; // Removes all attached attribute nodes.
  FAttributeList.free;
  FAttributeList := nil;
  FAttributeListing.free;
  if assigned(FCreatedElementsNodeLists)
    then for i := 0 to pred(FCreatedElementsNodeLists.Count) do
      TdomElementsNodeList(FCreatedElementsNodeLists[i]).free;
  if assigned(FCreatedElementsNodeListNSs)
    then for i := 0 to pred(FCreatedElementsNodeListNSs.Count) do
      TdomElementsNodeListNS(FCreatedElementsNodeListNSs[i]).free;
  FCreatedElementsNodeLists.free;
  FCreatedElementsNodeListNSs.free;
  inherited destroy;
end;

procedure TdomElement.doAttrModified(const originalTarget: TdomNode;
                                     const attrChange: TdomAttrChange;
                                     const relatedAttr: TdomAttr);
begin
  if assigned(referenceDocument)
    then referenceDocument.doAttrModified(originalTarget,attrChange,relatedAttr);
end;

procedure TdomElement.doBeforeClear;
var
  oldAttr: TdomAttr;
begin
  while hasAttributes do begin
    oldAttr := removeAttributeNode(Attributes.item(0) as TdomAttr);
    oldAttr.free;
  end;
end;

function TdomElement.getTagName: wideString;
begin
  Result:= NodeName;
end;

function TdomElement.getAttributes: TdomNamedNodeMap;
begin
  Result:= FAttributeList;
end;

function TdomElement.getAttributeLiteralValue(const name: wideString): wideString;
var
  attr: TdomAttr;
begin
  attr:= getAttributeNode(name);
    // Raises ENamespace_Err, if attributes.namespaceAware is 'true'.
  if assigned(attr)
    then result:= attr.nodeValue
    else result:= '';
end;

function TdomElement.getAttributeNode(const name: wideString): TdomAttr;
begin
  Result:= TdomAttr(Attributes.GetNamedItem(name));
    // Raises ENamespace_Err, if attributes.namespaceAware is 'true'.
end;

function TdomElement.getAttributeNodeNS(const namespaceURI,
                                              localName: wideString): TdomAttr;
begin
  Result:= TdomAttr(Attributes.GetNamedItemNS(namespaceURI,localName));
    // Raises ENamespace_Err, if attributes.namespaceAware is 'false'.
end;

function TdomElement.getAttributeNormalizedValue(const name: wideString): wideString;
var
  attr: TdomAttr;
begin
  attr:= getAttributeNode(name);  // Raises ENamespace_Err, if attributes.namespaceAware is 'true'.
  if assigned(attr) then begin
    try
      result:= attr.value;
    except
      raise EConvertError.Create('Literal attribute value cannot be resolved.');
    end;
  end else result:= '';
end;

function TdomElement.getAttributeNSLiteralValue(const namespaceURI,
                                                      localName: wideString): wideString;
var
  attr: TdomAttr;
begin
  attr:= getAttributeNodeNS(namespaceURI,localName); // Raises ENamespace_Err, if attributes.namespaceAware is 'false'.
  if assigned(attr)
    then result:= attr.nodeValue
    else result:= '';
end;

function TdomElement.getAttributeNSNormalizedValue(const namespaceURI,
                                                         localName: wideString): wideString;
var
  attr: TdomAttr;
begin
  attr:= getAttributeNodeNS(namespaceURI,localName); // Raises ENamespace_Err, if attributes.namespaceAware is 'false'.
  if assigned(attr) then begin
    try
      result:= attr.value;
    except
      raise EConvertError.Create('Literal attribute value cannot be resolved.');
    end;
  end else result:= '';
end;

function TdomElement.getElementsByTagName(const name: wideString): TdomNodeList;
var
  i: integer;
begin
  for i:= 0 to FCreatedElementsNodeLists.Count - 1 do
    if TdomElementsNodeList(FCreatedElementsNodeLists[i]).FQueryName = name
      then begin Result:= TdomElementsNodeList(FCreatedElementsNodeLists[i]); exit; end;
  Result:= TdomElementsNodeList.create(name,self);
  FCreatedElementsNodeLists.add(Result);
end;

function TdomElement.getElementsByTagNameNS(const namespaceURI,
                                                  localName: wideString): TdomNodeList;
var
  i: integer;
  nl: TdomElementsNodeListNS;
begin
  for i:= 0 to FCreatedElementsNodeListNSs.Count - 1 do begin
    nl:= TdomElementsNodeListNS(FCreatedElementsNodeListNSs[i]);
    if (nl.FQueryNamespaceURI = namespaceURI) and (nl.FQueryLocalName = localName)
      then begin Result:= nl; exit; end;
  end;
  Result:= TdomElementsNodeListNS.create(namespaceURI,localName,self);
  FCreatedElementsNodeListNSs.add(Result);
end;

function TdomElement.getExpandedName: wideString;
begin
  result:= NodeName;
end;

function TdomElement.getLocalName: wideString;
begin
  result:= FLocalName;
end;

function TdomElement.getNamespaceURI: wideString;
begin
  result:= FNamespaceURI;
end;

function TdomElement.getNodeName: wideString;
begin
  Result:= FNodeName;
end;

function TdomElement.getNodeType: TdomNodeType;
begin
  Result:= ntElement_Node;
end;

function TdomElement.getPrefix: wideString;
begin
  result:= FPrefix;
end;

function TdomElement.hasAttribute(const name: wideString): boolean;
begin
  Result:= assigned(Attributes.GetNamedItem(name));
end;

function TdomElement.hasAttributeNS(const namespaceURI,
                                          localName: wideString): boolean;
begin
  Result:= assigned(Attributes.GetNamedItemNS(namespaceURI,localName));
end;

function TdomElement.lookupNamespaceURI(const aPrefix: wideString): wideString;
var
  I: Integer;
begin
  if APrefix = '' then begin
    with Attributes do
      for I:= 0 to Pred(Length) do
        with TdomAttr(Item(I)) do
          if IsXmlnsDecl = NSDT_DEFAULT then begin
            Result:= NodeValue;
            Exit;
          end;
  end else begin
    with Attributes do
      for I:= 0 to Pred(Length) do
        with TdomAttr(Item(I)) do
          if (IsXmlnsDecl = NSDT_PREFIXED) and (LocalName = APrefix) then begin
            Result:= NodeValue;
            Exit;
          end;
  end;
  if Assigned(ParentNode)
    then Result := ParentNode.LookupNamespaceURI(APrefix)
    else Result := inherited LookupNamespaceURI(APrefix);
end;

procedure TdomElement.normalize;
var
  PrevNode, CurrentNode: TdomNode;
  i: integer;
begin
  {normalize text:}
  PrevNode:=nil;
  i:=ChildNodes.Length;
  while i>0 do
  begin
    Dec(i);
    CurrentNode:=ChildNodes.Item(i);
    if (CurrentNode.NodeType = ntText_Node) then
      begin
         if (Assigned(PrevNode)) and (PrevNode.NodeType = ntText_Node) then begin
            (CurrentNode as TdomText).AppendData((PrevNode as TdomText).Data);
            PrevNode.free;  // Removes and frees the node.
         end;
      end
    else  // no text node, then normalize
      CurrentNode.normalize;
    PrevNode:=CurrentNode;
  end;

  {normalize attributes:}
  for i:= 0 to attributes.Length-1 do
    attributes.item(i).normalize;
end;

function TdomElement.removeAttribute(const name: wideString): TdomAttr;
begin
  Result:= RemoveAttributeNode(GetAttributeNode(name));
     // GetAttributeNode() raises an ENamespace_Err if attributes.namespaceAware is 'true'.
     // RemoveAttributeNode() raises an ENo_Modification_Allowed_Err if readonly, ...
     // ... and an ENot_Found_Err if the node was not found.
end;

function TdomElement.removeAttributeNode(const oldAttr: TdomAttr): TdomAttr;
begin
  result:= Attributes.RemoveItem(oldAttr) as TdomAttr;
     // Raises an ENo_Modification_Allowed_Err if readonly, ...
     // ... and an ENot_Found_Err if the node was not found.

  doAttrModified(self,AC_REMOVAL,oldAttr);
end;

function TdomElement.removeAttributeNS(const namespaceURI,
                                             localName: wideString): TdomAttr;
begin
  Result:= removeAttributeNode(GetAttributeNodeNS(namespaceURI,localName));
     // GetAttributeNodeNS() raises ENamespace_Err if attributes.namespaceAware is 'false'.
     // RemoveAttributeNode() raises an ENo_Modification_Allowed_Err if readonly, ...
     // ... and an ENot_Found_Err if the node was not found.
end;

function TdomElement.resolveEntityReferences(const opt: TdomEntityResolveOption): integer;
var
  i: integer;
  hasEntRefs: boolean;
  child: TdomNode;
  docFrag: TdomDocumentFragment;
  error: TXmlErrorType;
  parser: TXmlToDomParser;
  replacementText: wideString;
begin
  result:= 0;
  case opt of
    erReplace: begin
      parser:= TXmlToDomParser.create(nil);
      try
        parser.DOMImpl:= referenceDocument.domImplementation;
        hasEntRefs:= false;
        i:= 0;
        while i < ChildNodes.Length do begin
          child:= ChildNodes.Item(i);
          if child.nodeType = ntEntity_Reference_Node then begin
            hasEntRefs:= true;

            referenceDocument.getReplacementText(child.nodeName, replacementText, error);
            if error in ET_WARNINGS then begin
              try
                docFrag:= referenceDocument.CreateDocumentFragment;
                try
                  if replacementText <> '' then
                    parser.parseWideString(replacementText, '', '', docFrag);
                  replaceChild(docFrag,child);
                  child.free;
                  dec(i); // Necessary, if an empty entity was referenced.
                finally
                  docFrag.free;
                end;
              except
                inc(result);
              end;
            end else
              inc(result);
          end else
            result := result + child.resolveEntityReferences(opt);
          inc(i);
        end; {while ...}
      finally
        parser.free;
      end;
      if hasEntRefs then normalize;
    end;
    erExpand: begin
      for i:= 0 to pred(ChildNodes.Length) do begin
        child:= ChildNodes.Item(i);
        if child.nodeType = ntEntity_Reference_Node then begin
          if not (child as TdomEntityReference).expand then
            inc(result);
        end else
          result := result + child.resolveEntityReferences(opt);
      end; {for ...}
    end;
  end;
end;

function TdomElement.setAttribute(const name,
                                        value: wideString): TdomAttr;
var
  attr: TdomAttr;
begin
  if isReadonly
    then raise ENo_Modification_Allowed_Err.create('No modification allowed error.');
  attr:= getAttributeNode(name);
    // Raises ENamespace_Err, if attributes.namespaceAware is 'true'.
  if assigned(attr) then begin
    attr.nodeValue:= value;
    result:= nil;
  end else begin
    result:= referenceDocument.CreateAttribute(name);
    result.nodeValue:= value; // Important: Set the nodeValue before adding the attribute to avoid double OnAttrModified event call.
    attributes.internalAdd(result);
    doAttrModified(self,AC_ADDITION,result);
  end;
end;

function TdomElement.setAttributeNS(const namespaceURI,
                                          qualifiedName,
                                          value: wideString): TdomAttr;
var
  attr: TdomAttr;
  prfx, localname: wideString;
begin
  if isReadonly
    then raise ENo_Modification_Allowed_Err.create('No modification allowed error.');
  if not xmlExtractPrefixAndLocalName(qualifiedName,prfx,localName) then begin
    if not IsXmlName(qualifiedName)
      then raise EInvalid_Character_Err.create('Invalid character error.')
      else raise ENamespace_Err.create('Namespace error.');
  end;
  if ( ((prfx = 'xmlns') or (qualifiedName = 'xmlns'))
    and not (namespaceURI ='http://www.w3.org/2000/xmlns/') )
      then raise ENamespace_Err.create('Namespace error.');
  if (namespaceURI = '') and (prfx <> '')
    then raise ENamespace_Err.create('Namespace error.');
  if (prfx = 'xml') and (namespaceURI <> 'http://www.w3.org/XML/1998/namespace')
    then raise ENamespace_Err.create('Namespace error.');
  attr:= getAttributeNodeNS(namespaceURI,localName);
    // Raises ENamespace_Err, if attributes.namespaceAware is 'false'.
  if assigned(attr) then begin
    attr.setPrefix(prfx);
    attr.nodeValue:= value;
    result:= nil;
  end else begin
    result:= referenceDocument.CreateAttributeNS(namespaceURI,qualifiedName);
    result.nodeValue:= value; // Important: Set the nodeValue before adding the attribute to avoid double OnAttrModified event call.
    attributes.internalAdd(result);
    doAttrModified(self,AC_ADDITION,result);
  end;
end;

function TdomElement.setAttributeNode(const newAttr: TdomAttr): TdomAttr;
var
  attrModified: boolean;
begin
  attrModified := newAttr.ownerElement = nil;
  result:= (attributes.setNamedItem(newAttr) as TdomAttr); // Raises all required exceptions.
  if attrModified
    then doAttrModified(self,AC_ADDITION,newAttr);
end;

function TdomElement.setAttributeNodeNS(const newAttr: TdomAttr): TdomAttr;
var
  attrModified: boolean;
begin
  attrModified := newAttr.ownerElement = nil;
  result:= (attributes.setNamedItemNS(newAttr) as TdomAttr); // Raises all required exceptions.
  if attrModified
    then doAttrModified(self,AC_ADDITION,newAttr);
end;

procedure TdomElement.setNodeValue(const value: wideString);
begin
  // Do nothing.
end;

procedure TdomElement.setPrefix(const value: wideString);
begin
  if not isXmlName(value)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  if isReadonly
    then raise ENo_Modification_Allowed_Err.create('No modification allowed error.');
  if not isXmlPrefix(value)
    then raise ENamespace_Err.create('Namespace error.');
  if namespaceURI = ''
    then raise ENamespace_Err.create('Namespace error.');
  if (value = 'xml') and (namespaceURI <> 'http://www.w3.org/XML/1998/namespace')
    then raise ENamespace_Err.create('Namespace error.');
  FPrefix:= value;
  FNodeName:= concat(value,':',localName);
end;

function TdomElement.validate2: boolean;
var
  I: Integer;
begin
  Result := inherited Validate2;

  // Validate attributes:
  for I := 0 to Pred(Attributes.Length) do
    if not Attributes.Item(I).Validate2 then
      Result := False;

  // Validate child nodes:
  for I := 0 to Pred(Childnodes.Length) do
    if not Childnodes.Item(I).Validate2 then
      Result := False;
end;

function TdomElement.validateIDREFS: boolean;
var
  I: Integer;
begin
  Result := True;

  // Validate attributes:
  for I := 0 to Pred(Attributes.Length) do
    if not Attributes.Item(I).ValidateIDREFS
      then Result := False;

  // Validate child elements:
  for I := 0 to Pred(Childnodes.Length) do
    if not Childnodes.Item(I).ValidateIDREFS then
      Result := False;
end;



//+++++++++++++++++++++++++++++ TdomText +++++++++++++++++++++++++++++++++
constructor TdomText.create(const aOwner: TdomDocument);
begin
  inherited create(aOwner);
  FNodeValue:= '';
  FAllowedChildTypes:= [];
end;

function TdomText.getIsWhitespaceInElementContent: boolean;
var
  pNode: TdomNode;
begin
  result:= false;
  if not (isXMLS(nodeValue) or (nodeValue = '')) then exit;
  pNode:= parentNode;
  while assigned(pNode) do begin
    case pNode.nodeType of
      ntElement_Node: begin
        result := referenceDocument.getContentType(pNode.nodeName) = AS_ELEMENT_CONTENTTYPE;
        break;
      end;
      ntEntity_Reference_Node:
        pNode:= pNode.parentNode;
    else
      break;
    end; {case ...}
  end; {while ...}
end;

function TdomText.getNodeName: wideString;
begin
  result:= '#text';
end;

function TdomText.getNodeType: TdomNodeType;
begin
  Result:= ntText_Node;
end;

function TdomText.SplitText(const offset: integer): TdomText;
begin
  if isReadonly
    then raise ENo_Modification_Allowed_Err.create('No modification allowed error.');
  if(offset < 0) or (offset > Length)
    then raise EIndex_Size_Err.create('Index size error.');
  Result:= referenceDocument.CreateTextNode(SubstringData(offset,length-offset));
  DeleteData(offset,length-offset);
  if assigned(ParentNode) then ParentNode.insertBefore(Result,self.NextSibling);
end;



//++++++++++++++++++++++++++++ TdomComment +++++++++++++++++++++++++++++++
constructor TdomComment.create(const aOwner: TdomDocument);
begin
  inherited create(aOwner);
  FNodeValue:= '';
  FAllowedChildTypes:= [];
end;

function TdomComment.getNodeName: wideString;
begin
  result:= '#comment';
end;

function TdomComment.getNodeType: TdomNodeType;
begin
  Result:= ntComment_Node;
end;



//+++++++++++++++++++++ TdomProcessingInstruction +++++++++++++++++++++++++
constructor TdomProcessingInstruction.create(const aOwner: TdomDocument;
                                             const targ: wideString);
begin
  if not IsXmlPITarget(targ)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  inherited create(aOwner);
  FTarget:= targ;
  FNodeValue:= '';
  FAllowedChildTypes:= [];
end;

procedure TdomProcessingInstruction.doCharacterDataModified;
begin
  if assigned(referenceDocument)
    then referenceDocument.doCharacterDataModified(self);
end;

function TdomProcessingInstruction.getData: wideString;
begin
  result:= FNodeValue;
end;

function TdomProcessingInstruction.getExpandedName: wideString;
begin
  result:= target;
end;

function TdomProcessingInstruction.getNodeName: wideString;
begin
  result:= target;
end;

function TdomProcessingInstruction.getNodeType: TdomNodeType;
begin
  Result:= ntProcessing_Instruction_Node;
end;

procedure TdomProcessingInstruction.setData(const value: wideString);
var
  prevValue: wideString;
begin
  prevValue:= NodeValue;
  NodeValue:= value;
  doCharacterDataModified;
end;



//++++++++++++++++++++++++++ TdomCDATASection +++++++++++++++++++++++++++++
constructor TdomCDATASection.create(const aOwner: TdomDocument);
begin
  inherited create(aOwner);
  FNodeValue:= '';
end;

function TdomCDATASection.getNodeName: wideString;
begin
  result:= '#cdata-section';
end;

function TdomCDATASection.getNodeType: TdomNodeType;
begin
  Result:= ntCDATA_Section_Node;
end;



//++++++++++++++++++++++++ TdomDocumentTypeDecl +++++++++++++++++++++++++++
constructor TdomDocumentTypeDecl.create(const aOwner: TdomDocument;
                                        const doctypeName,
                                              pubId,
                                              sysId,
                                              IntSubset: wideString);
begin
  inherited create(aOwner);
  FNodeName:= doctypeName;
  FNodeValue:= '';
  FPublicId:= pubId;
  FSystemId:= sysId;
  FInternalSubset:= IntSubset;
  FAllowedChildTypes:= [];
  FPreparationStatus := PS_UNPREPARED;
  FDtdModel:= TdomASModelCollection.create(self);
end;

procedure TdomDocumentTypeDecl.clear;
begin
  FDtdModel.ClearSubsets;  // Remark: FDtdModel itself is automatically freed.  // xxx Move ClearSubsets into TdomASModelCollection.destory ?
  inherited;
end;

function TdomDocumentTypeDecl.getInternalSubset: wideString;
begin
  Result:= FInternalSubset;
end;

function TdomDocumentTypeDecl.getName: wideString;
begin
  Result:= NodeName;
end;

function TdomDocumentTypeDecl.getNodeName: wideString;
begin
  result:= FNodeName;
end;

function TdomDocumentTypeDecl.getNodeType: TdomNodeType;
begin
  Result:= ntDocument_Type_Decl_Node;
end;

function TdomDocumentTypeDecl.getPublicId: wideString;
begin
  Result:= FPublicId;
end;

function TdomDocumentTypeDecl.getSystemId: wideString;
begin
  Result:= FSystemId;
end;

function TdomDocumentTypeDecl.prepare(const stopAtExtDecl: boolean;
                                      const intSubsetStartByteNumber,
                                            intSubsetStartCharNumber,
                                            intSubsetStartColumn,
                                            intSubsetStartLine: Int64): boolean;
const
  BUFFER_SIZE: integer = 4096;
var
  DtdToASParser: TDtdToASParser;
  documentUri: wideString;
  domImplementation: TDomImplementation;
begin
  if Assigned(ownerDocument) then begin
    documentUri := ownerDocument.documentUri;
    domImplementation := ownerDocument.domImplementation;
  end else begin
    documentUri := '';
    domImplementation := nil;
  end;
  FDtdModel.ClearSubsets;
  DtdToASParser := TDtdToASParser.create(nil);
  try
    DtdToASParser.DOMImpl := domImplementation;
    DtdToASParser.BufferSize := BUFFER_SIZE;
    result := DtdToASParser.parseDTD(internalSubset, documentUri,
                intSubsetStartByteNumber, intSubsetStartCharNumber,
                intSubsetStartColumn, intSubsetStartLine, publicId, systemId,
                FDtdModel);
    if result then begin
      if stopAtExtDecl
        then FPreparationStatus := PS_INCOMPLETE
        else FPreparationStatus := PS_SUCCESSFUL;
    end else FPreparationStatus := PS_UNSUCCESSFUL;
  finally
    DtdToASParser.free;
  end;
end;

procedure TdomDocumentTypeDecl.setNodeValue(const value: wideString);
begin
  // Do nothing.
end;



//++++++++++++++++++++++++++ TdomDocumentType +++++++++++++++++++++++++++++
constructor TdomDocumentType.create(const aOwner: TDomImplementation;
                                    const doctypeName,
                                          pubId,
                                          sysId: wideString);
begin
  inherited create(nil);
  FDomImpl:= aOwner;
  FNodeName:= doctypeName;
  FNodeValue:= '';
  FPublicId:= pubId;
  FSystemId:= sysId;
  FAllowedChildTypes:= [];
  FEntitiesListing:= TList.create;
  FEntitiesList:= TdomNamedNodeMap.create(self,FEntitiesListing,[ntEntity_Node],false);
  FNotationsListing:= TList.create;
  FNotationsList:= TdomNamedNodeMap.create(self,FNotationsListing,[ntNotation_Node],false);
end;

destructor TdomDocumentType.destroy;
begin
  clear;
  detachOwnerDocument;
  FEntitiesListing.free;
  FEntitiesList.free;
  FNotationsListing.free;
  FNotationsList.free;
  inherited destroy;
end;

procedure TdomDocumentType.attachOwnerDocument(const doc: TdomDocument);
begin
  if assigned(FOwnerDocument)
    then raise EWrong_Document_Err.create('Wrong document error.');
  FOwnerDocument:= doc;
  doc.FDoctype:= self;
end;

procedure TdomDocumentType.detachOwnerDocument;
begin
  if assigned(FOwnerDocument) then begin
    FOwnerDocument.FDoctype:= nil;
    FOwnerDocument:= nil;
  end;
end;

function TdomDocumentType.getEntities: TdomNamedNodeMap;
begin
  Result:= FEntitiesList;
end;

function TdomDocumentType.getInternalSubset: wideString;
begin
  Result:= '';  // By default return nothing.  Derived classes may override this method.
end;

function TdomDocumentType.getName: wideString;
begin
  Result:= NodeName;
end;

function TdomDocumentType.getNodeName: wideString;
begin
  result:= FNodeName;
end;

function TdomDocumentType.getNodeType: TdomNodeType;
begin
  Result:= ntDocument_Type_Node;
end;

function TdomDocumentType.getNotations: TdomNamedNodeMap;
begin
  Result:= FNotationsList;
end;

function TdomDocumentType.getPublicId: wideString;
begin
  Result:= FPublicId;
end;

function TdomDocumentType.getReadOnly: Boolean;
begin
  Result := true;
end;

function TdomDocumentType.getSystemId: wideString;
begin
  Result:= FSystemId;
end;

procedure TdomDocumentType.setNodeValue(const value: wideString);
begin
  // Do nothing.
end;




//++++++++++++++++++++++++++ TdomNotation ++++++++++++++++++++++++++++++
constructor TdomNotation.create(const aOwner: TdomDocument;
                                const name,
                                      pubId,
                                      sysId: wideString);
begin
  if not IsXmlName(name)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  if not isXmlSystemChars(sysId)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  if not isXmlPubidChars(publicId)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  inherited create(aOwner);
  FNodeName:= name;
  FNodeValue:= '';
  FPublicId:= pubId;
  FSystemId:= sysId;
  FAllowedChildTypes:= [];
end;

function TdomNotation.getNodeName: wideString;
begin
  result:= FNodeName;
end;

function TdomNotation.getNodeType: TdomNodeType;
begin
  Result:= ntNotation_Node;
end;

procedure TdomNotation.setNodeValue(const value: wideString);
begin
  // Do nothing.
end;



//+++++++++++++++++++++++++++ TdomEntity +++++++++++++++++++++++++++++++++
constructor TdomEntity.create(const aOwner: TdomDocument;
                              const name,
                                    pubId,
                                    sysId,
                                    notaName: wideString);
begin
  if not IsXmlName(name)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  if not isXmlSystemChars(sysId)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  if not isXmlPubidChars(pubId)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  inherited create(aOwner);
  FInputEncoding:= '';
  FNodeName:= name;
  FNotationName:= notaName;
  FPublicId:= pubId;
  FSystemId:= sysId;
  FXmlEncoding:= '';
  FXmlVersion:= '';
  FAllowedChildTypes:= [ntElement_Node,
                        ntProcessing_Instruction_Node,
                        ntComment_Node,
                        ntText_Node,
                        ntCDATA_Section_Node,
                        ntEntity_Reference_Node,
                        ntDocument_Fragment_Node];
end;

function TdomEntity.appendChild(const newChild: TdomNode): TdomNode;
begin
  if (publicId <> '') or (systemId <> '')
    then raise ENo_Modification_Allowed_Err.create('No modification allowed error.');
  Result:= inherited appendChild(newChild);
end;

function TdomEntity.getNodeName: wideString;
begin
  result:= FNodeName;
end;

function TdomEntity.getNodeType: TdomNodeType;
begin
  Result:= ntEntity_Node;
end;

function TdomEntity.getNotationName: wideString;
begin
  Result:= FNotationName;
end;

function TdomEntity.insertBefore(const newChild,
                                       refChild: TdomNode): TdomNode;
begin
  if (publicId <> '') or (systemId <> '')
    then raise ENo_Modification_Allowed_Err.create('No modification allowed error.');
  Result:= inherited insertBefore(newChild,refChild);
end;

function TdomEntity.replaceChild(const newChild,
                                       oldChild: TdomNode): TdomNode;
begin
  if (publicId <> '') or (systemId <> '')
    then raise ENo_Modification_Allowed_Err.create('No modification allowed error.');
  Result:= inherited replaceChild(newChild,oldChild);
end;

procedure TdomEntity.setNodeValue(const value: wideString);
begin
  // Do nothing
end;



//++++++++++++++++++++++++ TdomEntityReference +++++++++++++++++++++++++
constructor TdomEntityReference.create(const aOwner: TdomDocument;
                                       const name: wideString);
begin
  if not IsXmlName(name)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  inherited create(aOwner);
  setReadOnly(true);
  FNodeName:= name;
  FNodeValue:= '';
  FAllowedChildTypes:= [ntElement_Node,
                        ntText_Node,
                        ntCDATA_Section_Node,
                        ntEntity_Reference_Node,
                        ntProcessing_Instruction_Node,
                        ntComment_Node,
                        ntDocument_Fragment_Node];
end;

function TdomEntityReference.expand: boolean;
// Resolves the replacement text of the entity reference by the value of
// its corresponding TdomASEntityDecl object or by a default entity
// respectivly.  Returns 'true' if successful, otherwise 'false'.   // xxx Change return type to TXmlErrorType?
var
  error: TXmlErrorType;
  parser: TXmlToDomParser;
  replacementText: wideString;
begin
  setReadOnly(false);
  try
    clear;

    referenceDocument.getReplacementText(nodeName, replacementText, error);
    result := error in ET_WARNINGS;
    if result and (replacementText <> '') then begin
      parser:= TXmlToDomParser.create(nil);
      try
        parser.DOMImpl:= referenceDocument.domImplementation;
        parser.parseWideString(replacementText, '', '', self);
      finally
        parser.free;
      end;
    end;

    makeChildrenReadonly;

  finally
    setReadOnly(true);
  end;
end;

function TdomEntityReference.getNodeName: wideString;
begin
  result:= FNodeName;
end;

function TdomEntityReference.getNodeType: TdomNodeType;
begin
  Result:= ntEntity_Reference_Node;
end;

function TdomEntityReference.validate2: boolean;
var
  I: Integer;
begin
  Result := inherited Validate2;

  // Validate child nodes:
  for I := 0 to Pred(Childnodes.Length) do
    if not Childnodes.Item(I).Validate2 then
      Result := False;
end;

procedure TdomEntityReference.SetNodeValue(const value: wideString);
begin
  // Do nothing.
end;

function TdomEntityReference.CloneNode(const deep: boolean): TdomNode;
begin
  result:= inherited cloneNode(deep);
end;



//++++++++++++++++++++++++ TdomDocumentFragment +++++++++++++++++++++++++++
constructor TdomDocumentFragment.create(const aOwner: TdomDocument);
begin
  inherited create(aOwner);
  FNodeValue:= '';
  FAllowedChildTypes:= [ntElement_Node,
                        ntText_Node,
                        ntCDATA_Section_Node,
                        ntEntity_Reference_Node,
                        ntProcessing_Instruction_Node,
                        ntComment_Node,
                        ntDocument_Fragment_Node];
end;

function TdomDocumentFragment.getAbsoluteIndex: integer;
begin
  Result := -1;
end;

function TdomDocumentFragment.getLevel: integer;
begin
  Result := -1;
end;

function TdomDocumentFragment.getNodeName: wideString;
begin
  result:= '#document-fragment';
end;

function TdomDocumentFragment.getNodeType: TdomNodeType;
begin
  Result:= ntDocument_Fragment_Node;
end;

function TdomDocumentFragment.resolveEntityReferences(const opt: TdomEntityResolveOption): integer;
var
  i: integer;
begin
  result:= 0;
  for i:= 0 to pred(ChildNodes.Length) do
    result := result + ChildNodes.Item(i).resolveEntityReferences(opt);
end;

procedure TdomDocumentFragment.SetNodeValue(const value: wideString);
begin
  // Do nothing.
end;



//+++++++++++++++++++++++++ TdomXPathNamespace ++++++++++++++++++++++++++++
constructor TdomXPathNamespace.create(const aOwnerSet: TdomXPathNodeSetResult;
                                      const aOwnerElement: TdomElement;
                                      const aNamespaceUri,
                                            aPrefix: wideString);
begin
  if not ( IsXmlPrefix(aPrefix) or (aPrefix = '') )
    then raise EInvalid_Character_Err.create('Invalid character error.');
  if ( (aPrefix = 'xmlns') and not (aNamespaceURI = 'http://www.w3.org/2000/xmlns/') )
    then raise ENamespace_Err.create('Namespace error.');
  if (aNamespaceURI = '') and (aPrefix <> '')
    then raise ENamespace_Err.create('Namespace error.');
  if (aPrefix = 'xml') and (aNamespaceURI <> 'http://www.w3.org/XML/1998/namespace')
    then raise ENamespace_Err.create('Namespace error.');
  inherited create(aOwnerSet);
  FAllowedChildTypes:= [];
  setReadOnly(true);
  FNamespaceURI:= aNamespaceUri;
  FOwnerElement:= aOwnerElement;
  FPrefix:= aPrefix;
end;

function TdomXPathNamespace.getDocument: TdomDocument;
begin
  if assigned(FOwnerElement)
    then result:= FOwnerElement.referenceDocument
    else result:= nil;
end;

function TdomXPathNamespace.getExpandedName: wideString;
begin
  Result:= FPrefix;
end;

function TdomXPathNamespace.getLocalName: wideString;
begin
  Result:= FPrefix;
end;

function TdomXPathNamespace.getNamespaceURI: wideString;
begin
  Result:= FNamespaceURI;
end;

function TdomXPathNamespace.getNodeName: wideString;
begin
  Result:= '#namespace';
end;

function TdomXPathNamespace.getNodeType: TdomNodeType;
begin
  Result:= ntXPath_Namespace_Node;
end;

function TdomXPathNamespace.getNodeValue: wideString;
begin
  Result:= FNamespaceURI;
end;

function TdomXPathNamespace.getOwnerElement: TdomElement;
begin
  result:= FOwnerElement;
end;

function TdomXPathNamespace.getOwnerSet: TdomXPathNodeSetResult;
begin
  Result:= (GetOwner as TdomXPathNodeSetResult);
end;

function TdomXPathNamespace.getPrefix: wideString;
begin
  result:= FPrefix;
end;

function TdomXPathNamespace.lookupNamespaceURI(const aPrefix: wideString): wideString;
begin
  if Assigned(OwnerElement)
    then Result := OwnerElement.LookupNamespaceURI(APrefix)
    else Result := inherited LookupNamespaceURI(APrefix);
end;



//++++++++++++++++++++++++++++ TdomDocument +++++++++++++++++++++++++++++++
constructor TdomDocument.create(const aOwner: TDomImplementation;
                                const aDoctype: TdomDocumentType);
begin
  if assigned(aDoctype) then begin
    if not assigned(aOwner)
      then raise EWrong_Document_Err.create('Wrong document error.');
    if assigned(aDoctype.referenceDocument) or (aOwner.documentTypes.IndexOf(aDoctype) = -1)
      then raise EWrong_Document_Err.create('Wrong document error.');
  end;
  inherited create(nil);
  FDomImpl:= aOwner;
  if assigned(aDoctype)
    then aDoctype.attachOwnerDocument(self);
  FNodeValue:= '';
  FSystemId:= '';
  FXmlEncoding:= '';
  FXmlStandalone:= STANDALONE_UNSPECIFIED;
  FXmlVersion:= '';
  FModified:= false;
  FDefaultView:= nil;
{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
  FASModelsNS:= TdomASModelCollectionNS.create(self);
{$endif}
  FCreatedNodeIterators:= TList.create;
  FCreatedTreeWalkers:= TList.create;
  FCreatedElementsNodeLists:= TList.create;
  FCreatedElementsNodeListNSs:= TList.create;
  FIDs:= TUtilsWideStringList.create;
  FIDs.Sorted:= true;
  FIDs.Duplicates:= dupError;
  FAllowedChildTypes:= [ntElement_Node,
                        ntProcessing_Instruction_Node,
                        ntComment_Node,
                        ntDocument_Type_Decl_Node,
                        ntDocument_Fragment_Node];
end;

destructor TdomDocument.destroy;
begin
  doBeforeClear; // Frees all elementNodeLists, nodeIterators and treeWalkers.
{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
  FASModelsNS.free;
{$endif}
  FCreatedNodeIterators.free;
  FCreatedTreeWalkers.free;
  FCreatedElementsNodeLists.free;
  FCreatedElementsNodeListNSs.free;
  FIDs.free;
  if assigned(doctype)
    then doctype.detachOwnerDocument;
  inherited destroy;
end;

function TdomDocument.appendChild(const newChild: TdomNode): TdomNode;
begin
  if not Assigned(NewChild) then
    raise ENot_Supported_Err.create('Not supported error.');
  case NewChild.NodeType of
    ntElement_Node:
      if Assigned(DocumentElement) then
        raise EHierarchy_Request_Err.create('Hierarchy request error.');
    ntDocument_Type_Decl_Node:
      if Assigned(DocumentElement) or Assigned(DoctypeDecl) then
        raise EHierarchy_Request_Err.create('Hierarchy request error.');
  end;
  Result := inherited AppendChild(NewChild);
end;

procedure TdomDocument.clearInvalidNodeIterators;
var
  i: integer;
begin
  for i:= 0 to FCreatedNodeIterators.count-1 do
  if TdomNodeIterator(FCreatedNodeIterators[i]).FInvalid then begin
    TdomNodeIterator(FCreatedNodeIterators[i]).free;
    FCreatedNodeIterators[i]:= nil;
  end;
  FCreatedNodeIterators.pack;
  FCreatedNodeIterators.Capacity:= FCreatedNodeIterators.Count;
end;

function TdomDocument.createElement(const tagName: wideString): TdomElement;
begin
  result:= TdomElement.create(self,tagName);
end;

function TdomDocument.createEntity(const name,
                                         pubId,
                                         sysId,
                                         notaName: wideString): TdomEntity;
begin
  result:= TdomEntity.create(self,name,pubId,sysId,notaName);
end;

function TdomDocument.createAttribute(const name: wideString): TdomAttr;
begin
  Result:= TdomAttr.create(self,name,true);
end;

function TdomDocument.createAttributeNS(const namespaceURI,
                                              qualifiedName: wideString): TdomAttr;
begin
  Result:= TdomAttr.createNS(self,namespaceURI,qualifiedName,true);
end;

function TdomDocument.createCDATASection(const data: wideString): TdomCDATASection;
begin
  Result:= TdomCDATASection.create(self);
  Result.Data:= Data;
end;

function TdomDocument.createComment(const data: wideString): TdomComment;
begin
  Result:= TdomComment.create(self);
  Result.Data:= Data;
end;

function TdomDocument.createDocumentFragment: TdomDocumentFragment;
begin
  Result:= TdomDocumentFragment.create(self);
end;

function TdomDocument.createDocumentTypeDecl(const name,
                                                   pubId,
                                                   sysId,
                                                   intSubset: wideString): TdomDocumentTypeDecl;
begin
  Result:= TdomDocumentTypeDecl.create(self,name,pubId,sysId,intSubset);
end;

function TdomDocument.createElementNS(const namespaceURI,
                                            qualifiedName: wideString): TdomElement;
begin
  result:= TdomElement.createNS(self,namespaceURI,qualifiedName);
end;

function TdomDocument.createEntityReference(const name: wideString): TdomEntityReference;
begin
  Result:= TdomEntityReference.create(self,name);
end;

function TdomDocument.createNotation(const name,
                                           pubId,
                                           sysId: wideString): TdomNotation;
begin
  Result := TdomNotation.Create(self,name,pubId,sysId);
end;

function TdomDocument.createProcessingInstruction(const targ,
                                                        data : wideString): TdomProcessingInstruction;
begin
  Result := TdomProcessingInstruction.Create(self,targ);
  Result.Data:= Data;
end;

function TdomDocument.createTextNode(const data: wideString): TdomText;
begin
  Result := TdomText.Create(self);
  Result.Data:= Data;
end;

procedure TdomDocument.doAttrModified(const sourceNode: TdomNode;
                                      const attrChange: TdomAttrChange;
                                      const relatedAttr: TdomAttr);
begin
  FModified:= true;
  try
    if assigned(FOnAttrModified)
      then FOnAttrModified(self,sourceNode,attrChange,relatedAttr);
  finally
    if assigned(domImplementation)
      then domImplementation.doAttrModified(sourceNode,attrChange,relatedAttr);
  end;
end;

procedure TdomDocument.doBeforeClear;
var
  I : Integer;
begin
  for I := 0 to Pred(FCreatedNodeIterators.Count) do
    TdomNodeIterator(FCreatedNodeIterators[I]).Free;
  FCreatedNodeIterators.Clear;
  for I := 0 to Pred(FCreatedTreeWalkers.Count) do
    TdomTreeWalker(FCreatedTreeWalkers[I]).Free;
  FCreatedTreeWalkers.Clear;
  for I := 0 to Pred(FCreatedElementsNodeLists.Count) do
    TdomElementsNodeList(FCreatedElementsNodeLists[I]).Free;
  FCreatedElementsNodeLists.Clear;
  for I := 0 to Pred(FCreatedElementsNodeListNSs.Count) do
    TdomElementsNodeListNS(FCreatedElementsNodeListNSs[I]).Free;
  FCreatedElementsNodeListNSs.Clear;

{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
  FASModelsNS.Clear;
{$endif}
end;

procedure TdomDocument.doCharacterDataModified(node: TdomNode);
begin
  FModified := true;
  try
    if assigned(FOnCharacterDataModified)
      then FOnCharacterDataModified(self,node);
  finally
    if assigned(domImplementation)
      then domImplementation.doCharacterDataModified(node);
  end;
end;

procedure TdomDocument.doNodeClearing(node: TdomNode);
begin
  FModified := true;
  try
    if assigned(FOnNodeClearing)
      then FOnNodeClearing(self, node);
  finally
    if assigned(domImplementation)
      then domImplementation.doNodeClearing(node);
    notifyIterators(node, neClearing);
  end;
end;

procedure TdomDocument.doNodeInserted(node: TdomNode);
begin
  FModified := true;
  try
    if assigned(FOnNodeInserted)
      then FOnNodeInserted(self,node);
  finally
    if assigned(domImplementation)
      then domImplementation.doNodeInserted(node);
  end;
end;

procedure TdomDocument.doNodeRemoving(node: TdomNode);
begin
  FModified := true;
  try
    if assigned(FOnNodeRemoving)
      then FOnNodeRemoving(self,node);
  finally
    if assigned(domImplementation)
      then domImplementation.doNodeRemoving(node);
    notifyIterators(node,neRemoving);
  end;
end;

function TdomDocument.getAbsoluteIndex: integer;
begin
  Result := 0;
end;

function TdomDocument.getLevel: integer;
begin
  Result := 0;
end;

function TdomDocument.getNodeName: wideString;
begin
  Result := '#document';
end;

function TdomDocument.getNodeType: TdomNodeType;
begin
  Result := ntDocument_Node;
end;

function TdomDocument.getReferenceDocument: TdomDocument;
begin
  Result := self;
end;

procedure TdomDocument.getReplacementText(const entityName: wideString;
                                            out replText: wideString;
                                            out error: TXmlErrorType);
begin
  if assigned(DtdModel) then begin
    Error := DtdModel.findASEntityReplacementText(entityName, replText);
  end else begin
    if EntityName = 'lt' then begin
      replText := '&#60;';
      Error := ET_NONE;
    end else if EntityName = 'gt' then begin
      replText := #62;
      Error := ET_NONE;
    end else if EntityName = 'amp' then begin
      replText := '&#38;';
      Error := ET_NONE;
    end else if EntityName = 'apos' then begin
      replText := #39;
      Error := ET_NONE;
    end else if EntityName = 'quot' then begin
      replText := #34;
      Error := ET_NONE;
    end else begin
      replText := '';
      Error := ET_ENTITY_DECL_NOT_FOUND;
    end;
  end;
end;

function TdomDocument.hasAttrDef(const elementType,
                                       attributeName: wideString): boolean;
begin
  if Assigned(DtdModel)
    then result := Assigned(DtdModel.findASAttributeDecl(elementType, attributeName))
    else result := False;
end;

function TdomDocument.hasAttrEnum(const elementType,
                                        attributeName,
                                        attributeValue: wideString): boolean;
var
  i: Integer;
begin
  if Assigned(DtdModel) then begin
    result := false;
    with DtdModel.findASAttributeDecl(elementType, attributeName) do begin
      if enumeration.count = 0 then
        result := true;
      for i := 0 to pred(enumeration.count) do
        if enumeration[i] = attributeValue then begin
          result:= true;
          break;
        end;
    end;
  end else
    Result := False;
end;

function TdomDocument.hasUnparsedEntity(const entityName: wideString): boolean;
var
  entityDecl: TdomASEntityDecl;
begin
  if Assigned(DtdModel) then begin
    entityDecl := DtdModel.findASEntityDecl(entityName);
    if assigned(entityDecl)
      then result := not entityDecl.isParsedEntity
      else result := false;
  end else
    Result := False;
end;

function TdomDocument.importNode(const importedNode: TdomNode;
                                 const deep: boolean): TdomNode;
var
  I: Integer;
  UserDataEvent: TdomUserDataEvent;
begin
  Result := ImportNode2(ImportedNode, Deep);

  // Call user data event handlers:
  with ImportedNode do
    if Assigned(Result) and Assigned(FUserData) then
      with FUserData do
        for I := 0 to Pred(Count) do begin
          @UserDataEvent := Pointer(FUserDataHandlers[I]);
          if Assigned(UserDataEvent) then
            UserDataEvent(OT_NODE_IMPORTED, WideStrings[I], Objects[I], ImportedNode, Result);
        end;
end;

function TdomDocument.importNode2(const importedNode: TdomNode;
                                  const deep: boolean): TdomNode;
var
  i: integer;
  newChild: TdomNode;
  oldAttr: TdomAttr;
begin
  if not assigned(importedNode)
    then raise ENot_Supported_Err.create('Not supported error.');
  case importedNode.NodeType of
    ntAttribute_Node:
      with importedNode do begin
        if isNamespaceNode
          then result := createAttributeNS(namespaceURI,nodeName)
          else result := createAttribute(nodeName);
        result.FNodeValue := FNodeValue;
      end;
    ntCDATA_Section_Node:
      Result := createCDATASection((importedNode as TdomCDATASection).Data);
    ntComment_Node:
      Result := createComment((importedNode as TdomComment).Data);
    ntDocument_Fragment_Node:
      begin
        Result := createDocumentFragment;
        if deep then for i:= 0 to pred(importedNode.ChildNodes.Length) do begin
          newChild := importNode(importedNode.ChildNodes.Item(i),true);
          Result.appendChild(newChild);
        end;
      end;
    ntElement_Node:
      begin
        with importedNode do begin
          if isNamespaceNode then begin
            result:= createElementNS(namespaceURI,nodeName);
            // Duplicating specified attributes:
            for i := 0 to importedNode.attributes.Length-1 do begin
              oldAttr := TdomAttr(importedNode.attributes.Item(i));
              if oldAttr.specified then begin
                newChild := importNode(oldAttr,true);
                (result as TdomElement).setAttributeNodeNS((newChild as TdomAttr));
              end;
            end; {for ...}
          end else begin
            result:= createElement(nodeName);
            // Duplicating specified attributes:
            for i := 0 to importedNode.attributes.Length-1 do begin
              oldAttr := TdomAttr(importedNode.attributes.Item(i));
              if oldAttr.specified then begin
                newChild := importNode(oldAttr,true);
                (result as TdomElement).setAttributeNode((newChild as TdomAttr));
              end;
            end; {for ...}
          end;
        end; {if ... else ...}

        // Duplicating child nodes:
        if deep then for i := 0 to pred(importedNode.ChildNodes.Length) do begin
          newChild:= importNode(importedNode.ChildNodes.Item(i),true);
          Result.appendChild(newChild);
        end;
      end;
    ntEntity_Node:
      with (importedNode as TdomEntity) do begin
        result := createEntity(nodeName,publicId,systemId,notationName);
        (result as TdomEntity).inputEncoding:= inputEncoding;
        (result as TdomEntity).xmlEncoding:= xmlEncoding;
        (result as TdomEntity).xmlVersion:= xmlVersion;
        if deep then for i := 0 to pred(childNodes.length) do begin
          newChild := importNode(childNodes.Item(i),true);
          result.appendChild(newChild);
        end;
      end;
    ntEntity_Reference_Node:
      begin
        Result := createEntityReference(importedNode.NodeName);
        (result as TdomEntityReference).expand;
      end;
    ntNotation_Node:
      with (importedNode as TdomEntity) do begin
        result := createNotation(nodeName,publicId,systemId);
      end;
    ntProcessing_Instruction_Node:
        Result := createProcessingInstruction((importedNode as TdomProcessingInstruction).target,
                                              (importedNode as TdomProcessingInstruction).data);
    ntText_Node:
      Result := createTextNode((importedNode as TdomText).Data);
  else
    raise ENot_Supported_Err.create('Not supported error.');
  end;
end;

function TdomDocument.getAttrType(const elementType,
                                        attributeName: wideString): TXmlDataType;
var
  attrDecl: TdomASAttributeDecl;
begin
  if assigned(DtdModel) then begin
    attrDecl:= DtdModel.findASAttributeDecl(elementType, attributeName);
    if assigned(attrDecl) 
      then result := attrDecl.attrType
      else result := AS_STRING_DATATYPE;
  end else result := AS_STRING_DATATYPE;
end;

function TdomDocument.getBaseUri: wideString;
begin
  if IsUriAbsoluteURIWideStr(documentUri)
    then Result := documentUri
    else Result := '';
end;

function TdomDocument.getContentType(const elementType: wideString): TdomASContentType;
var
  elmtDecl: TdomASElementDecl;
begin
  if Assigned(DtdModel) then begin
    elmtDecl:= DtdModel.findASElementDecl(elementType);
    if assigned(elmtDecl)
      then result := elmtDecl.contentType
      else result := AS_UNKNOWN_CONTENTTYPE;
  end else result := AS_UNKNOWN_CONTENTTYPE;
end;

function TdomDocument.getDoctypeDecl: TdomDocumentTypeDecl;
var
  Child: TdomNode;
begin
  Result:= nil;
  Child := FirstChild;
  while assigned(Child) do begin
    if Child.NodeType = ntDocument_Type_Decl_Node then begin
      Result := (Child as TdomDocumentTypeDecl);
      break;
    end;
    Child := Child.NextSibling;
  end;
end;

function TdomDocument.getDocumentElement: TdomElement;
begin
  Result := FindFirstChildElement;
end;

procedure TdomDocument.initDoc(const tagName: wideString);
begin
  if not IsXmlName(tagName)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  if assigned (DocumentElement)
    then raise EHierarchy_Request_Err.create('Hierarchy request error.');
  appendChild(CreateElement(tagName));
end;

procedure TdomDocument.initDocNS(const namespaceURI,
                                       qualifiedName: wideString);
var
  prfx, localName: wideString;
begin
  if not IsXmlName(qualifiedName)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  if not xmlExtractPrefixAndLocalName(qualifiedName,prfx,localName) then begin
    if not IsXmlName(qualifiedName)
      then raise EInvalid_Character_Err.create('Invalid character error.')
      else raise ENamespace_Err.create('Namespace error.');
  end;
  if ( ((prfx = 'xmlns') or (qualifiedName = 'xmlns'))
    and not (namespaceURI ='http://www.w3.org/2000/xmlns/') )
      then raise ENamespace_Err.create('Namespace error.');
  if (namespaceURI = '') and (prfx <> '')
    then raise ENamespace_Err.create('Namespace error.');
  if (prfx = 'xml') and (namespaceURI <> 'http://www.w3.org/XML/1998/namespace')
    then raise ENamespace_Err.create('Namespace error.');
  if assigned (DocumentElement)
    then raise EHierarchy_Request_Err.create('Hierarchy request error.');
  appendChild(CreateElementNS(namespaceURI,qualifiedName));
end;

function TdomDocument.insertBefore(const newChild,
                                         refChild: TdomNode): TdomNode;
begin
  if not Assigned(NewChild) then
    raise ENot_Supported_Err.create('Not supported error.');
  case NewChild.NodeType of
    ntElement_Node: begin
      if Assigned(DocumentElement) then
        raise EHierarchy_Request_Err.create('Hierarchy request error.');
      if Assigned(DoctypeDecl) then
        if ChildNodes.IndexOf(DoctypeDecl) >= ChildNodes.IndexOf(RefChild) then
          raise EHierarchy_Request_Err.create('Hierarchy request error.');
    end;
    ntDocument_Type_Decl_Node: begin
      if Assigned(DocumentElement) then
        if ChildNodes.IndexOf(DocumentElement) < ChildNodes.IndexOf(RefChild) then
          raise EHierarchy_Request_Err.create('Hierarchy request error.');
      if Assigned(DoctypeDecl) then
        raise EHierarchy_Request_Err.create('Hierarchy request error.');
    end;
  end;
  Result := inherited InsertBefore(NewChild, RefChild);
end;

procedure TdomDocument.notifyIterators(const node: TdomNode;
                                       const eventType: TdomNodeEvent);
var
  i: integer;
begin
  for i := 0 to pred(FCreatedNodeIterators.count) do
    TdomNodeIterator(FCreatedNodeIterators[i]).handleNodeEvent(node,eventType);
end;

function TdomDocument.precalculateNormalizedAttrValue(const S: wideString): wideString;
// This function performs the first steps of attribute value normalization (see
// XML 1.0, § 3.3.3).  All line breaks in 'S' must have been normalized to #xA.
// The function starts with a normalized value consisting of the empty string.
// For each character, entity reference, or character reference in the
// unnormalized attribute value, beginning with the first and continuing to the
// last, it does the following:
// - For a character reference, it appends the referenced character to the
//   normalized value.
// - For an entity reference, it recursively applies this step to the
//   replacement text of the entity.
// - For a white space character (#x20, #xD, #xA, #x9), it appends a space
//   character (#x20) to the normalized value.
// - For another character, it appends the character to the normalized value.
const
  SPACE: WideChar = #$20;  // ' '
type
  TKindOfToken = (IS_TEXT, IS_REFSTART, IS_CHARREF, IS_ENTITYREF);
var
  error: TXmlErrorType;
  i: integer;
  kindOfToken: TKindOfToken;
  referenceName: wideString;
  replacementText: wideString;
  text: TUtilsCustomWideStr;
begin
  text:= TUtilsCustomWideStr.create;
  try

    kindOfToken:= IS_TEXT;
    for i:= 1 to length(S) do begin
      case kindOfToken of
        IS_TEXT:
          if IsXmlWhiteSpace(S[i]) then begin // White space?
            text.addWideChar(SPACE)
          end else if S[i] = '&'
            then kindOfToken:= IS_REFSTART
            else text.addWideChar(S[i]);
        IS_REFSTART:
          if S[i] = '#' then begin
            referenceName:= '&#';
            kindOfToken:= IS_CHARREF;
          end else begin
            kindOfToken:= IS_ENTITYREF;
            referenceName:= wideString(S[i]);
          end;
        IS_CHARREF:
          if S[i] = ';' then begin
            referenceName:= concat(referenceName,';');
            text.addWideString(XmlCharRefToStr(referenceName));
            kindOfToken:= IS_TEXT;
          end else referenceName:= concat(referenceName,wideString(S[i]));
        IS_ENTITYREF:
          if S[i] = ';' then begin
            getReplacementText(referenceName, replacementText, error);
            if Error = ET_NONE then begin
              text.addWideString(precalculateNormalizedAttrValue(replacementText));
            end else
              raise EConvertError.CreateFmt('&%S; cannot be resolved.',[referenceName]);
            kindOfToken:= IS_TEXT;
          end else referenceName:= concat(referenceName,wideString(S[i]));
      end; {case ...}
    end; {for ...}
    case kindOfToken of
      IS_REFSTART, IS_CHARREF, IS_ENTITYREF:
        raise EConvertError.Create('Malformed literal attribute value.');
    end; {case ...}

    result:= text.value;

  finally
    text.Free;
  end;
end;

function TdomDocument.calculateNormalizedAttrValue(
  const Attr: TdomAttr): wideString;
var
  DummyStr: wideString;
begin
  Assert(Attr.referenceDocument = self);

  Result := precalculateNormalizedAttrValue(Attr.NodeValue);

  // Further attribute normalization (See XML 1.0, § 3.3.3):
  if Assigned(Attr.OwnerElement) then
    if not (GetAttrType(Attr.OwnerElement.NodeName, Attr.Nodename) = AS_STRING_DATATYPE) then begin
      DummyStr := Result;
      Result := NormalizeSpace(DummyStr);
    end;
end;

function TdomDocument.refersToExternalEntity(const S: wideString): boolean;
type
  TKindOfToken = (IS_TEXT, IS_REFSTART, IS_CHARREF, IS_ENTITYREF);
var
  i: integer;
  kindOfToken: TKindOfToken;
  referenceName: wideString;
begin
  result:= false;
  kindOfToken:= IS_TEXT;
  for i:= 1 to length(S) do begin
    case kindOfToken of
      IS_TEXT:
        if S[i] = '&'
          then kindOfToken:= IS_REFSTART;
      IS_REFSTART:
        if S[i] = '#' then begin
          kindOfToken:= IS_CHARREF;
        end else begin
          kindOfToken:= IS_ENTITYREF;
          referenceName:= wideString(S[i]);
        end;
      IS_CHARREF:
        if S[i] = ';'
          then kindOfToken:= IS_TEXT;
      IS_ENTITYREF:
        if S[i] = ';' then begin
          if not assigned(DtdModel) then
            raise EConvertError.create('Entity reference cannot be resolved.');
          result:= DtdModel.refersToExternalEntity(referenceName);
          if result then exit;
          kindOfToken:= IS_TEXT;
        end else referenceName:= concat(referenceName,wideString(S[i]));
    end; {case ...}
  end; {for ...}
  case kindOfToken of
    IS_REFSTART, IS_CHARREF, IS_ENTITYREF:
      raise EConvertError.Create('Malformed literal attribute value.');
  end; {case ...}
end;

function TdomDocument.refersToLTEntity(const S: wideString): boolean;
// Tests whether the attribute refers to an entity whose replacement text
// contains a '<' character.
type
  TKindOfToken = (IS_TEXT, IS_REFSTART, IS_CHARREF, IS_ENTITYREF);
var
  entDecl: TdomASEntityDecl;
  i: integer;
  kindOfToken: TKindOfToken;
  referenceName: wideString;
begin
  result:= false;
  kindOfToken:= IS_TEXT;
  for i:= 1 to length(S) do begin
    case kindOfToken of
      IS_TEXT:
        if S[i] = '&'
          then kindOfToken:= IS_REFSTART;
      IS_REFSTART:
        if S[i] = '#' then begin
          kindOfToken:= IS_CHARREF;
        end else begin
          kindOfToken:= IS_ENTITYREF;
          referenceName:= wideString(S[i]);
        end;
      IS_CHARREF:
        if S[i] = ';'
          then kindOfToken:= IS_TEXT;
      IS_ENTITYREF:
        if S[i] = ';' then begin
          if assigned(DtdModel)
            then entDecl:= DtdModel.findASEntityDecl(referenceName)
            else entDecl:= nil;
          if assigned(entDecl) then begin
            result:= DtdModel.refersToLTEntity(entDecl);
          end else if isXmlPredefinedEntityName(referenceName) then begin
            result := false;
          end else raise EConvertError.create('Entity reference cannot be resolved.');
          if result then exit;
          kindOfToken:= IS_TEXT;
        end else referenceName:= concat(referenceName,wideString(S[i]));
    end; {case ...}
  end; {for ...}
  case kindOfToken of
    IS_REFSTART, IS_CHARREF, IS_ENTITYREF:
      raise EConvertError.Create('Malformed literal attribute value.');
  end; {case ...}
end;

procedure TdomDocument.setNodeValue(const value: wideString);
begin
  // Do nothing.
end;

procedure TdomDocument.freeTreeWalker(var TreeWalker: TdomTreeWalker);
var
  TreeWalkerIndex: integer;
begin
  if not assigned(TreeWalker) then exit;
  TreeWalkerIndex := FCreatedTreeWalkers.IndexOf(TreeWalker);
  if TreeWalkerIndex = -1
    then raise EWrong_Document_Err.create('Wrong document error.');
  FCreatedTreeWalkers.Delete(TreeWalkerIndex);
  TreeWalker.free;
  TreeWalker := nil;
end;

function TdomDocument.getDtdModel: TdomASModelCollection;
begin
  if Assigned(DoctypeDecl)
    then Result := DoctypeDecl.dtdModel
    else Result := nil;
end;

function TdomDocument.getElementById(const elementId: wideString): TdomElement;
var
  index: integer;
begin
  if IDs.find(elementId,index)
    then result := TdomAttr(IDs.objects[index]).ownerElement
    else result := nil;
end;

function TdomDocument.getElementsByTagName(const tagName: wideString): TdomNodeList;
var
  i: integer;
begin
  for i := 0 to FCreatedElementsNodeLists.Count - 1 do
    if TdomElementsNodeList(FCreatedElementsNodeLists[i]).FQueryName = tagName
      then begin Result := TdomElementsNodeList(FCreatedElementsNodeLists[i]); exit; end;
  Result := TdomElementsNodeList.create(tagName,self);
  FCreatedElementsNodeLists.add(Result);
end;

function TdomDocument.getElementsByTagNameNS(const namespaceURI,
                                                   localName: wideString): TdomNodeList;
var
  i: integer;
  nl: TdomElementsNodeListNS;
begin
  for i := 0 to FCreatedElementsNodeListNSs.Count - 1 do begin
    nl := TdomElementsNodeListNS(FCreatedElementsNodeListNSs[i]);
    if (nl.FQueryNamespaceURI = namespaceURI) and (nl.FQueryLocalName = localName)
      then begin Result := nl; exit; end;
  end;
  Result := TdomElementsNodeListNS.create(namespaceURI,localName,self);
  FCreatedElementsNodeListNSs.add(Result);
end;

function TdomDocument.prepareAttributes: boolean;
begin
  if Assigned(DocumentElement)
    then Result := PrepareAttributes2(DocumentElement)
    else Result := True;
end;

function TdomDocument.prepareAttributes2(const node: TdomNode): boolean;
var
  attr: TdomAttr;
  attrDecl: TdomASAttributeDecl;
  defaultAttrList: TUtilsNameValueList;
  i: integer;
  newAttr: TdomAttr;
  textAttr: TdomAttr;
begin
  with node do begin

    if nodeType = ntElement_Node then begin

      if isNamespaceNode then
        result := False;


      // Step 1: Remove all TdomAttr nodes attached to this element whose
      //         'specified' property is 'false'.

      for i:= pred(attributes.length) downto 0 do begin
        attr:= attributes.item(i) as TdomAttr;
        if not attr.specified then
          attr.free;  // Removes and frees the attribute node.
      end;


      // Step 2: Create and add missing fixed and default TdomAttr nodes with
      //         'specified' set to 'false'.

      if Assigned(DtdModel) then begin
        defaultAttrList:= DtdModel.CreateDefaultAttrList(nodeName, result);
                          // Creates a list containing name-value pairs of fixed and
                          // default attributes for the specified element type.
        try
          with defaultAttrList do
            for i:= 0 to pred(length) do begin
              textAttr:= (node as TdomElement).getAttributeNode(Names[i]);
              if not assigned(textAttr) then begin
                newAttr:= CreateAttribute(attrDecl.name);
                newAttr.nodeValue:= Values[i];
                newAttr.FSpecified:= false;
                (node as TdomElement).SetAttributeNode(newAttr);
              end;
            end;
        finally
          defaultAttrList.Free;
        end;
      end;

    end;

    // Step 3: Prepare the attributes of the child nodes.

    for i:= 0 to pred(childnodes.length) do
      if not prepareAttributes2(childnodes.item(i)) then
        result := False;
  end;
end;

function TdomDocument.replaceChild(const newChild,
                                         oldChild: TdomNode): TdomNode;
begin
  if not ( Assigned(NewChild) and Assigned(OldChild) ) then
    raise ENot_Supported_Err.create('Not supported error.');
  case newChild.NodeType of
    ntElement_Node: begin
      if Assigned(DocumentElement) and (DocumentElement <> OldChild) then
        raise EHierarchy_Request_Err.create('Hierarchy request error.');
      if Assigned(DoctypeDecl) then
        if ChildNodes.IndexOf(DoctypeDecl) > ChildNodes.IndexOf(OldChild) then
          raise EHierarchy_Request_Err.create('Hierarchy request error.');
    end;
    ntDocument_Type_Decl_Node: begin
      if Assigned(DoctypeDecl) and (DoctypeDecl <> OldChild) then
        raise EHierarchy_Request_Err.create('Hierarchy request error.');
      if Assigned(DocumentElement) then
        if ChildNodes.IndexOf(DocumentElement) < ChildNodes.IndexOf(OldChild) then
          raise EHierarchy_Request_Err.create('Hierarchy request error.');
    end;
  end;
  Result:= inherited ReplaceChild(NewChild, OldChild);
end;

function TdomDocument.resolveEntityReferences(const opt: TdomEntityResolveOption): integer;
begin
  if assigned(documentElement)
    then result := documentElement.resolveEntityReferences(opt)
    else result := 0;
end;

function TdomDocument.CreateNodeIterator(const root: TdomNode;
                                               whatToShow: TdomWhatToShow;
                                               nodeFilter: TdomNodeFilter;
                                               entityReferenceExpansion: boolean): TdomNodeIterator;
begin
  Result:= TdomNodeIterator.create(root,whatToShow,nodeFilter,entityReferenceExpansion);
  FCreatedNodeIterators.add(Result);
end;

function TdomDocument.CreateTreeWalker(const root: TdomNode;
                                             whatToShow: TdomWhatToShow;
                                             nodeFilter: TdomNodeFilter;
                                             entityReferenceExpansion: boolean): TdomTreeWalker;
begin;
  Result:= TdomTreeWalker.create(root,whatToShow,nodeFilter,entityReferenceExpansion);
  FCreatedTreeWalkers.add(Result);
end;

function TdomDocument.validate(const opt: TdomEntityResolveOption;
                               const intSubsetStartByteNumber,
                                     intSubsetStartCharNumber,
                                     intSubsetStartColumn,
                                     intSubsetStartLine: Int64): boolean;
var
  I: Integer;
begin
  if not Assigned(DtdModel) then begin
    Result := False;
    SendErrorNotification(ET_DOCTYPE_NOT_FOUND, self);
    Exit;
  end;

  Result := True;
  if Assigned(DoctypeDecl) then
    if DoctypeDecl.PreparationStatus <> PS_SUCCESSFUL then
      Result := DoctypeDecl.Prepare(False, IntSubsetStartByteNumber,
                  IntSubsetStartCharNumber, IntSubsetStartColumn,
                  IntSubsetStartLine);

  Result := DtdModel.Validate and Result;

  if not Assigned(DocumentElement) then begin
    Result := False;
    SendErrorNotification(ET_ROOT_NOT_FOUND, Self);
  end;

  if ResolveEntityReferences(Opt) > 0 then begin
    Result := False;
    SendErrorNotification(ET_UNRESOLVABLE_ENTITY_REFERENCE, Self);
  end;

  // VC: Root Element Type (XML 1.0, § 2.8)
  if Assigned(DoctypeDecl) then begin
    if DoctypeDecl.Name <> DocumentElement.NodeName then begin
      Result := false;
      SendErrorNotification(ET_WRONG_ROOT_ELEMENT_TYPE, Self);
    end;
  end;

  IDs.Clear;
  for I := 0 to Pred(Childnodes.Length) do begin
    if not Childnodes.Item(I).Validate2 then
      Result := False;
  end;
  if Result then begin
    // VC: IDREF (XML 1.0, § 3.3.1)
    // Second parse only for IDREF and IDREFS:
    if not ValidateIDREFS then
      Result := False;
  end else IDs.Clear;
end;

function TdomDocument.validateAttr(const node: TdomAttr): TXmlErrorType;
const
  SPACE: WideChar  = #$20;
  LT:    WideChar  = #$3C;  // '<'
var
  i,startIndex,indexCount: integer;
  attriValue: wideString;
begin
  result:= ET_NONE;
  with node do begin
    try
      attriValue:= Value;
    except
      // VC: Entity declared (XML 1.0, § 4.1)
      result:= ET_ENTITY_DECL_NOT_FOUND;
      exit;
    end;

    // WFC: No External Entity Reference (XML 1.0, § 3.1)
    try
      if refersToExternalEntity(nodeValue) then begin
        result:= ET_ATTRIBUTE_VALUE_REFERS_TO_EXTERNAL_ENTITY;
        exit;
      end;
    except
      on EConvertError do begin
        result:= ET_ATTRIBUTE_VALUE_REFERS_TO_EXTERNAL_ENTITY;
        exit;
      end;
    end;

    // WFC: No < in Attribute Values (XML 1.0, § 3.1)
    try
      if refersToLTEntity(nodeValue) then begin
        result:= ET_LT_IN_ATTRIBUTE_VALUE;
        exit;
      end;
    except
      on EConvertError do begin
        result:= ET_LT_IN_ATTRIBUTE_VALUE;
        exit;
      end;
    end;

    // VC: Attribute Value Type (XML 1.0, § 3.1)
    if assigned(OwnerElement) then begin
      if hasAttrDef(OwnerElement.nodeName,nodename) then begin
        case getAttrType(OwnerElement.nodeName,nodename) of

          AS_STRING_DATATYPE:
          begin
            // VC: XML Schema Part 2: Datatypes: Strings
            if not isXMLChars(attriValue) then
              result:= ET_ATTRIBUTE_TYPE_MISMATCH;
          end;

          AS_NOTATION_DATATYPE:
          begin
            // VC: Notation Attributes (XML 1.0, § 3.3.1)
            if not hasAttrEnum(OwnerElement.nodeName, nodename, attriValue) then
              result:= ET_ATTRIBUTE_TYPE_MISMATCH;
          end;

          AS_ID_DATATYPE:
          begin
            if isXMLName(attriValue) then begin
              try
                IDs.addObject(attriValue,node);
              except
                // VC: ID (XML 1.0, § 3.3.1)
                on EStringListError do begin
                  result:= ET_DUPLICATE_ID_VALUE;
                  exit;
                end;
              end;
            end else
              // VC: Entity (XML 1.0, § 3.3.1)
              result:= ET_ATTRIBUTE_TYPE_MISMATCH;
          end;

          AS_ENTITY_DATATYPE:
          begin
            // VC: Entity Name (XML 1.0, § 3.3.1)
            if isXMLName(attriValue) then begin
              if not hasUnparsedEntity(attriValue) then
                result:= ET_TARGET_UNPARSED_ENTITY_NOT_FOUND;
            end else
              result:= ET_ATTRIBUTE_TYPE_MISMATCH;
          end;

          AS_ENTITIES_DATATYPE:
          begin
            // VC: Entity Name (XML 1.0, § 3.3.1)
            if isXMLNames(attriValue) then begin
              startIndex:= 1; indexCount:= 0;
              for i:= 1 to length(attriValue) do begin
                if attriValue[i] = SPACE then begin
                  if not hasUnparsedEntity(copy(attriValue,startIndex,IndexCount)) then begin
                    result:= ET_TARGET_UNPARSED_ENTITY_NOT_FOUND;
                    exit;
                  end;
                  startIndex:= succ(i);
                  indexCount:= 0;
                end else inc(indexCount);
              end;
              if not hasUnparsedEntity(copy(attriValue,startIndex,IndexCount)) then
                result:= ET_TARGET_UNPARSED_ENTITY_NOT_FOUND;
            end else
              result:= ET_ATTRIBUTE_TYPE_MISMATCH;
          end;

          AS_NMTOKEN_DATATYPE:
          begin
            if isXmlNmtoken(attriValue) then begin
              // VC: Enumeration (XML 1.0, § 3.3.1)
              if not hasAttrEnum(OwnerElement.nodeName, nodename, attriValue) then
                result:= ET_ATTRIBUTE_TYPE_MISMATCH;
            end else
              // VC: Name Token (XML 1.0, § 3.3.1)
              result:= ET_ATTRIBUTE_TYPE_MISMATCH;
          end;

          AS_NMTOKENS_DATATYPE:
          begin
            // VC: Name Token (XML 1.0, § 3.3.1)
            if not isXmlNmtokens(attriValue) then
              result:= ET_ATTRIBUTE_TYPE_MISMATCH;
          end;

        end; {case ...}

      end else
        result:= ET_ATTRIBUTE_DEFINITION_NOT_FOUND;
    end else
      result := ET_ATTRIBUTE_DEFINITION_NOT_FOUND;
  end;
end;

function TdomDocument.validateDefaultAttr(const node: TdomElement): TXmlErrorType;
var
  attrDecl: TdomASAttributeDecl;
  attributeNames: TUtilsWideStringList;
  dummy: integer;
  elmDecl: TdomASElementDecl;
  error: TXmlErrorType;
  i, j: integer;
  newAttr: TdomAttr;
  newAttrValue: wideString;
  ReadOnlyBackup: Boolean;
  textAttr: TdomAttr;
begin
  result:= ET_NONE;

  with node do begin

    if Assigned(DtdModel)
      then elmDecl:= DtdModel.findASElementDecl(nodeName)
      else elmDecl:= nil;

    // VC: Element Valid (XML 1.0, § 3)
    if not assigned(elmDecl) then begin
      result:= ET_ELEMENT_TYPE_DECL_NOT_FOUND;
      exit;
    end;

    // Remark:  There may be more than one attribute declaration for the same
    //          element type in subsequent content models.  Therefore we need
    //          to keep track of which attributes have already been declared
    //          in order to skip later declarations.  This is done by the
    //          'attributeNames' list.

    attributeNames:= TUtilsWideStringList.create;
    try
      attributeNames.Sorted:= true;
      attributeNames.Duplicates:= dupError;
      with DtdModel do begin
        for i:= 0 to pred(length) do begin
          elmDecl:= items[i].findASElementDecl(nodeName);
          if assigned(elmDecl) then begin
            with elmDecl.attributeDecls do begin
              for j:= 0 to pred(length) do begin
                attrDecl:= item(j) as TdomASAttributeDecl;
                if not attributeNames.find(attrDecl.name,dummy) then begin  // If duplicate than skip
                  attributeNames.add(attrDecl.name);

                  case attrDecl.constraintType of

                    AVC_DEFAULT:
                    begin
                      textAttr:= getAttributeNode(attrDecl.name);
                      if not assigned(textAttr) then begin
                        newAttrValue:= self.DtdModel.normalizeAttributeDeclValue(attrDecl,error);
                        if error = ET_NONE then begin
                          newAttr:= CreateAttribute(attrDecl.name);
                          newAttr.nodeValue:= newAttrValue;
                          newAttr.FSpecified:= false;
                          ReadOnlyBackup := node.IsReadOnly;
                          node.SetReadOnly(False);
                          try
                            node.SetAttributeNode(newAttr);
                          finally
                            node.SetReadOnly(ReadOnlyBackup);
                          end;
                        end else begin
                          result:= error;
                          exit;
                        end;
                      end;
                    end;

                    AVC_FIXED: // VC: Fixed Attribute Default (XML 1.0, § 3.3.2)
                    begin
                      newAttrValue:= self.DtdModel.normalizeAttributeDeclValue(attrDecl,error);
                      if error = ET_NONE then begin
                        textAttr:= getAttributeNode(attrDecl.name);
                        if assigned(textAttr) then begin
                          if textAttr.value <> newAttrValue then begin
                            result:= ET_FIXED_ATTRIBUTE_MISMATCH;
                            exit;
                          end;
                        end else begin
                          newAttr:= CreateAttribute(attrDecl.name);
                          newAttr.nodeValue:= newAttrValue;
                          newAttr.FSpecified:= false;
                          ReadOnlyBackup := node.IsReadOnly;
                          node.SetReadOnly(False);
                          try
                            node.SetAttributeNode(newAttr);
                          finally
                            node.SetReadOnly(ReadOnlyBackup);
                          end;
                        end;
                      end else begin
                        result:= error;
                        exit;
                      end;
                    end;

                    AVC_REQUIRED: // VC: Required Attribute (XML 1.0, § 3.3.2)
                    begin
                      if not hasAttribute(attrDecl.name) then begin
                        result:= ET_REQUIRED_ATTRIBUTE_NOT_FOUND;
                        exit;
                      end;
                    end;

                  end; {case ...}

                end; {if ...}
              end; {for ...}
            end; {with ...}
          end; {if ...}
        end; {for ...}
      end; {with ...}
    finally
      attributeNames.free;
    end;
  end;
end;

function TdomDocument.validateElement(const node: TdomElement): TXmlErrorType;
var
  contentModelOk: boolean;
  elementNames: TUtilsWideStringList;
  elmDecl: TdomASElementDecl;
  index: integer;
  isNonDeterministic: boolean;
  nodeToTest: TdomNode;
  ok: boolean;
  treeWalker: TdomTreeWalker;
begin
  with node do begin

    if Assigned(DtdModel)
      then elmDecl:= DtdModel.findASElementDecl(nodeName)
      else elmDecl:= nil;

    // VC: Element Valid (XML 1.0, § 3)
    if not assigned(elmDecl) then begin
      result:= ET_ELEMENT_TYPE_DECL_NOT_FOUND;
      exit;
    end;

    treeWalker := CreateTreeWalker( node,
                                    // Hide entity reference nodes:
                                    [ntElement_Node,
                                    ntAttribute_Node,
                                    ntText_Node,
                                    ntCDATA_Section_Node,
                                    ntEntity_Node,
                                    ntProcessing_Instruction_Node,
                                    ntComment_Node,
                                    ntDocument_Node,
                                    ntDocument_Type_Node,
                                    ntDocument_Fragment_Node,
                                    ntNotation_Node,
                                    ntDocument_Type_Decl_Node],
                                    nil,
                                    true );
    try
      case elmDecl.ContentType of

        AS_EMPTY_CONTENTTYPE:
        begin
          nodeToTest:= treeWalker.firstChild;
          while assigned(nodeToTest) do begin
            if not ( (nodeToTest.nodeType = ntText_Node) and (nodeToTest.nodeValue = '') ) then begin
              result:= ET_ELEMENT_DECLARED_EMPTY_HAS_CONTENT;
              exit;
            end;
            nodeToTest:= treeWalker.nextSibling;
          end; {while ...}
        end;

        AS_ELEMENT_CONTENTTYPE:
        begin
          elementNames:= TUtilsWideStringList.create;
          try
            ok:= true;
            nodeToTest:= treeWalker.firstChild;
            while assigned(nodeToTest) do begin
              with nodeToTest do begin
                case nodeType of
                  ntElement_Node:
                    elementnames.Add(NodeName);
                  ntText_Node:
                    if not (IsXmlS(nodeValue) or (nodeValue = '')) then begin
                      ok:= false;
                      break;
                    end;
                  ntProcessing_Instruction_Node,ntComment_Node:; // Do nothing --> node accepted.
                  else begin
                    ok:= false;
                    break;
                  end;
                end; {case ...}
              end; {with ...}
              nodeToTest:= treeWalker.nextSibling;
            end; {while ...}
            if ok then begin
              if not assigned(elmDecl.contentModel)
                then raise EParserException.create('Internal Parser error.');
              index:= 0;
              contentModelOk:= elmDecl.contentModel.validateNames(elementnames,index,isNonDeterministic);
              if isNonDeterministic then begin
                result:= ET_NONDETERMINISTIC_ELEMENT_CONTENT_MODEL;
                exit;
              end else ok:= contentModelOk and (index = elementnames.count);
            end; {if ok ...}
          finally
            elementnames.free;
          end;
          if not ok then begin
            result:= ET_ELEMENT_WITH_ILLEGAL_ELEMENT_CONTENT;
            exit;
          end; {if ...}
        end;

        AS_MIXED_CONTENTTYPE:
        begin
          elementnames:= TUtilsWideStringList.create;
          try
            ok:= true;
            nodeToTest:= treeWalker.firstChild;
            while assigned(nodeToTest) do begin
              with nodeToTest do begin
                case nodeType of
                  ntElement_Node:
                    elementnames.Add(NodeName);
                  ntText_Node, ntProcessing_Instruction_Node, ntComment_Node, ntCDATA_Section_Node:; // Do nothing --> node accepted.
                  else begin
                    ok:= false;
                    break;
                  end;
                end; {case ...}
              end; {with ...}
              nodeToTest:= treeWalker.nextSibling;
            end; {while ...}

            if ok then begin
              if not assigned(elmDecl.contentModel)
                then raise EParserException.create('Internal Parser error.');
              if (elmDecl.contentModel.contentModelType = AS_CHOICE_CM) and
                 (elmDecl.contentModel.subModels.length = 0) then begin  // Is PCDATA only?
                if elementnames.count > 0 then begin
                  result:= ET_ELEMENT_WITH_ILLEGAL_ELEMENT_CONTENT;
                  exit;
                end;
              end else begin
                index:= 0;
                contentModelOk:= elmDecl.contentModel.validateNames(elementnames,index,isNonDeterministic);
                if isNonDeterministic then begin
                  result:= ET_NONDETERMINISTIC_ELEMENT_CONTENT_MODEL;
                  exit;
                end else ok:= contentModelOk and (index = elementnames.count);
              end; {if ... else ...}
            end; {if ok ...}

          finally
            elementnames.free;
          end;
          if not ok then begin
            result:= ET_ELEMENT_WITH_ILLEGAL_ELEMENT_CONTENT;
            exit;
          end; {if ...}
        end;

        AS_STRICT_MIXED_CONTENTTYPE:  // xxx Add support for AS_STRICT_MIXED_CONTENTTYPE
          raise EParserException.create('AS_STRICT_MIXED_CONTENTTYPE is currently not supported by TdomElement.validate.');

      end; {case ...}
    finally
      freeTreeWalker(TreeWalker);
    end; {try ...}

    // Validate default and fixed attributes:
    Result := ValidateDefaultAttr(node);

  end;
end;

function TdomDocument.validateEntityRef(const node: TdomEntityReference): TXmlErrorType;
var
  entDecl: TdomASEntityDecl;
begin
  result:= ET_NONE;

  with node do begin
    if not isXmlPredefinedEntityName(nodeName) then begin

      // VC: Entity declared (XML 1.0, § 4.1)
      if Assigned(DtdModel)
        then entDecl:= DtdModel.findASEntityDecl(nodeName)
        else entDecl:= nil;
      if not assigned(entDecl) then begin
        result:= ET_ENTITY_DECL_NOT_FOUND;
        exit;
      end;

      if entDecl.isParsedEntity then begin

        if not entDecl.resolve then begin
          result:= ET_UNRESOLVABLE_ENTITY_REFERENCE;
          exit;
        end;

        // WFC: Parsed Entity (XML 1.0, § 4.1)
        if DtdModel.refersToUnparsedEntity(entDecl) then begin
          result:= ET_REFERS_TO_UNPARSED_ENTITY;
          exit;
        end;

        // WFC: No Recursion (XML 1.0, § 4.1)
        if DtdModel.refersToItself(entDecl,true) then begin
          result:= ET_RECURSIVE_REFERENCE;
          exit;
        end;

        // WFC: Well-Formed Parsed Entities (XML 1.0, § 4.3.2)
        if entDecl.usability = AS_UNUSABLE then begin
          result:= ET_NO_PROPER_MARKUP_REFERENCED;
          exit;
        end; {if ...}

      end else
        result:= ET_REFERS_TO_UNPARSED_ENTITY;

    end; {if ...}
  end;
end;

function TdomDocument.validateIDREFS: boolean;
begin
  Result := DocumentElement.ValidateIDREFS;
end;

function TdomDocument.validateNode(const node: TdomNode): TXmlErrorType;
begin
  if Node is TdomElement then begin
    Result := ValidateElement(TdomElement(Node));
  end else if Node is TdomAttr then begin
    Result := ValidateAttr(TdomAttr(Node));
  end else if Node is TdomEntityReference then begin
    Result := ValidateEntityRef(TdomEntityReference(Node));
  end else
    Result := ET_NONE;
end;



//+++++++++++++++++++++++++ TdomASObjectList +++++++++++++++++++++++++++++
constructor TdomASObjectList.create;
begin
  inherited create;
  FNodeList:= TList.create;
end;

destructor TdomASObjectList.destroy;
begin
  FNodeList.free;
  inherited;
end;

procedure TdomASObjectList.clear;
begin
  FNodeList.clear;
end;

function TdomASObjectList.appendASNode(const newNode: TdomASObject): TdomASObject;
begin
  FNodeList.Add(newNode);
  result:= newNode;
end;

procedure TdomASObjectList.Delete(const index: integer);
begin
  FNodeList.Delete(index);
end;

function TdomASObjectList.indexOf(const node: TdomASObject): integer;
begin
  result:= FNodeList.indexOf(node);
end;

function TdomASObjectList.getLength: integer;
begin
  Result:= FNodeList.count;
end;

function TdomASObjectList.insertBefore(const newNode,
                                             refNode: TdomASObject): TdomASObject;
begin
  Result:= newNode;
  with FNodeList do
    if assigned(refNode)
      then insert(indexOf(refNode),newNode)
      else add(newNode);
end;

function TdomASObjectList.removeASNode(const oldNode: TdomASObject): TdomASObject;
begin
  Result:= oldNode;
  FNodeList.Remove(oldNode);
end;

function TdomASObjectList.item(const index: integer): TdomASObject;
begin
  if (index < 0) or (index >= FNodeList.count)
    then Result:= nil
    else Result:= TdomASObject(FNodeList.Items[index]);
end;



//+++++++++++++++++++++++++ TdomASNamedObjectMap +++++++++++++++++++++++++
constructor TdomASNamedObjectMap.create(const aOwner: TdomASModel);
begin
  inherited create;
  FOwnerObject:= aOwner;
  FObjectList:= TList.create;
end;

destructor TdomASNamedObjectMap.destroy;
begin
  FObjectList.free;
  inherited;
end;

procedure TdomASNamedObjectMap.clear;
begin
  FObjectList.Clear;
end;

function TdomASNamedObjectMap.getLength: integer;
begin
  Result:= FObjectList.count;
end;

function TdomASNamedObjectMap.GetNamedItem(const name: wideString): TdomASObject;
var
  i: integer;
begin
  result:= nil;
  for i:= 0 to pred(FObjectList.count) do
    if (TdomASObject(FObjectList[i]).name = name) then begin
      Result:= TdomASObject(FObjectList[i]);
      break;
    end;
end;

function TdomASNamedObjectMap.item(const index: integer): TdomASObject;
begin
  if (index < 0) or (index >= FObjectList.count)
    then Result:= nil
    else Result:= TdomASObject(FObjectList.Items[index]);
end;

function TdomASNamedObjectMap.RemoveNamedItem(const name: wideString): TdomASObject;
begin
  Result:= getNamedItem(name);
  if not assigned(Result)
    then raise ENot_Found_Err.create('Not found error.');
  FObjectList.Remove(Result);
end;

function TdomASNamedObjectMap.SetNamedItem(const arg: TdomASObject): TdomASObject;
begin
  if assigned(GetNamedItem(arg.name))
    then Result:= RemoveNamedItem(arg.name)
    else Result:= nil;
  FObjectList.Add(arg);
end;



{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
//+++++++++++++++++++++++++ TdomASObjectListNS +++++++++++++++++++++++++++++
constructor TdomASObjectListNS.create;
begin
  inherited create;
  FNodeList:= TList.create;
end;

destructor TdomASObjectListNS.destroy;
begin
  FNodeList.free;
  inherited;
end;

procedure TdomASObjectListNS.clear;
begin
  FNodeList.clear;
end;

function TdomASObjectListNS.appendASNode(const newNode: TdomASObjectNS): TdomASObjectNS;
begin
  FNodeList.Add(newNode);
  result:= newNode;
end;

procedure TdomASObjectListNS.Delete(const index: integer);
begin
  FNodeList.Delete(index);
end;

function TdomASObjectListNS.indexOf(const node: TdomASObjectNS): integer;
begin
  result:= FNodeList.indexOf(node);
end;

function TdomASObjectListNS.getLength: integer;
begin
  Result:= FNodeList.count;
end;

function TdomASObjectListNS.insertBefore(const newNode,
                                             refNode: TdomASObjectNS): TdomASObjectNS;
begin
  Result:= newNode;
  with FNodeList do
    if assigned(refNode)
      then insert(indexOf(refNode),newNode)
      else add(newNode);
end;

function TdomASObjectListNS.removeASNode(const oldNode: TdomASObjectNS): TdomASObjectNS;
begin
  Result:= oldNode;
  FNodeList.Remove(oldNode);
end;

function TdomASObjectListNS.item(const index: integer): TdomASObjectNS;
begin
  if (index < 0) or (index >= FNodeList.count)
    then Result:= nil
    else Result:= TdomASObjectNS(FNodeList.Items[index]);
end;



//++++++++++++++++++++++++ TdomASNamedObjectMapNS ++++++++++++++++++++++++
constructor TdomASNamedObjectMapNS.create(const aOwner: TdomASModelNS);
begin
  inherited create;
  FOwnerObject:= aOwner;
  FObjectList:= TList.create;
end;

destructor TdomASNamedObjectMapNS.destroy;
begin
  FObjectList.free;
  inherited;
end;

procedure TdomASNamedObjectMapNS.clear;
begin
  FObjectList.Clear;
end;

function TdomASNamedObjectMapNS.getLength: integer;
begin
  Result:= FObjectList.count;
end;

function TdomASNamedObjectMapNS.GetNamedItem(const namespaceURI,
                                                   localName: wideString): TdomASObjectNS;
var
  i: integer;
begin
  result:= nil;
  for i:= 0 to pred(FObjectList.count) do
    if (TdomASObjectNS(FObjectList[i]).namespaceURI = namespaceURI)
      and (TdomASObjectNS(FObjectList[i]).localName = localName) then begin
      Result:= TdomASObjectNS(FObjectList[i]);
      break;
    end;
end;

function TdomASNamedObjectMapNS.item(const index: integer): TdomASObjectNS;
begin
  if (index < 0) or (index >= FObjectList.count)
    then Result:= nil
    else Result:= TdomASObjectNS(FObjectList.Items[index]);
end;

function TdomASNamedObjectMapNS.RemoveNamedItem(const namespaceURI,
                                                      localName: wideString): TdomASObjectNS;
begin
  Result:= getNamedItem(namespaceURI,localName);
  if not assigned(Result)
    then raise ENot_Found_Err.create('Node not found error.');
  FObjectList.Remove(Result);
end;

function TdomASNamedObjectMapNS.SetNamedItem(const arg: TdomASObjectNS): TdomASObjectNS;
begin
  if assigned(GetNamedItem(arg.namespaceURI,arg.localName))
    then Result:= RemoveNamedItem(arg.namespaceURI,arg.localName)
    else Result:= nil;
  FObjectList.Add(arg);
end;
{$endif}



//++++++++++++++++++++++ TdomASModelCollection ++++++++++++++++++++++++++
constructor TdomASModelCollection.create(const aOwner: TdomDocumentTypeDecl);
begin
  if not assigned(aOwner) then
    raise ENot_Supported_Err.create('Not supported error.');
  inherited create(aOwner);
  FASModels:= TList.Create;

  FASModels.Add(TdomASModel.create(self)); // Internal Subset
  FASModels.Add(TdomASModel.create(self)); // External Subset
end;

destructor TdomASModelCollection.destroy;
begin
  FASModels.Free;
  inherited;
end;

procedure TdomASModelCollection.clearSubsets;
begin
  InternalSubset.Clear;
  ExternalSubset.Clear;
end;

function TdomASModelCollection.createDefaultAttrList(const elementType: wideString;
                                                       out listComplete: boolean): TUtilsNameValueList;
// Creates a TUtilsNameValueList object containing name-value pairs of all
// default or fixed attributes of the specified element type as declared in
// the associated ASModel object.
//
//         Remark:  There may be more than one attribute declaration for
//         the same element type in subsequent content models.  Therefore
//         we need to keep track of which attributes have already been
//         declared in order to skip later declarations.  This is done by
//         the 'attributeNames' list.
var
  attrDecl: TdomASAttributeDecl;
  attributeNames: TUtilsWideStringList;
  attValue: wideString;
  elmDecl: TdomASElementDecl;
  error: TXmlErrorType;
  dummy, i, j: integer;
begin
  listComplete := True;

  attributeNames:= TUtilsWideStringList.create;
  try
    attributeNames.Sorted:= true;
    attributeNames.Duplicates:= dupError;

    Result:= TUtilsNameValueList.Create;
    try
      Result.Sorted:= true;
      for i:= 0 to pred(length) do begin

        elmDecl:= items[i].findASElementDecl(elementType);
        if assigned(elmDecl) then
          with elmDecl.attributeDecls do

            for j:= 0 to pred(length) do begin
              attrDecl:= item(j) as TdomASAttributeDecl;
              if not attributeNames.find(attrDecl.name,dummy) then begin  // If duplicate than skip
                attributeNames.add(attrDecl.name);

                if attrDecl.constraintType in [AVC_DEFAULT, AVC_FIXED] then begin
                  attValue := normalizeAttributeDeclValue(attrDecl, error);
                  if error = ET_NONE
                    then result.Add(attrDecl.name, attValue)
                    else listComplete := False;
                end;

              end; {if ...}
            end; {for ...}

      end; {for ...}
    except
      Result.Free;
      Result := nil;
      raise;
    end;

  finally
    attributeNames.Free;
  end;
end;

function TdomASModelCollection.findASAttributeDecl(const elementName,
                                                         attributeName: wideString): TdomASAttributeDecl;
var
  i: integer;
begin
  result:= nil;
  with ASModels do
    for i:= 0 to pred(length) do begin
      result:= TdomASModel(Items[i]).findASAttributeDecl(elementName,attributeName);
      if assigned(result) then break;
    end;
end;

function TdomASModelCollection.findASElementDecl(const name: wideString): TdomASElementDecl;
var
  i: integer;
begin
  result:= nil;
  with ASModels do
    for i:= 0 to pred(length) do begin
      result:= TdomASModel(Items[i]).findASElementDecl(name);
      if assigned(result) then break;
    end;
end;

function TdomASModelCollection.findASEntityDecl(const name: wideString): TdomASEntityDecl;
var
  i: integer;
begin
  result:= nil;
  with ASModels do
    for i:= 0 to pred(length) do begin
      result:= TdomASModel(Items[i]).findASEntityDecl(name);
      if assigned(result) then break;
    end;
end;

function TdomASModelCollection.findASEntityReplacementText(const entityName: wideString;
                                                             out replText: wideString): TXmlErrorType;
var
  entDecl: TdomASEntityDecl;
begin
  entDecl:= findASEntityDecl(EntityName);
  if assigned(entDecl) then begin
    if not entDecl.isParsedEntity then begin
      replText := '';
      result := ET_REFERS_TO_UNPARSED_ENTITY;
    end else if not entDecl.resolve then begin
      replText := '';
      result := ET_UNRESOLVABLE_ENTITY_REFERENCE;
    end else if entDecl.usability = AS_UNUSABLE then begin  // Remark: Must go after 'entDecl.resolve'.
      replText := '';
      result := ET_NO_PROPER_MARKUP_REFERENCED;
    end else if refersToItself(entDecl, false) then begin  // xxx Change 'true' to 'false'?
      replText := '';
      result := ET_RECURSIVE_REFERENCE;
    end else begin
      replText := entDecl.replacementText;
      result := ET_NONE;
    end;
  end else begin
    if EntityName = 'lt' then begin
      replText := '&#60;';
      result := ET_NONE;
    end else if EntityName = 'gt' then begin
      replText := #62;
      result := ET_NONE;
    end else if EntityName = 'amp' then begin
      replText := '&#38;';
      result := ET_NONE;
    end else if EntityName = 'apos' then begin
      replText := #39;
      result := ET_NONE;
    end else if EntityName = 'quot' then begin
      replText := #34;
      result := ET_NONE;
    end else begin
      replText := '';
      result := ET_ENTITY_DECL_NOT_FOUND;
    end;
  end;
end;

function TdomASModelCollection.findASNotationDecl(const name: wideString): TdomASNotationDecl;
var
  i: integer;
begin
  result:= nil;
  with ASModels do
    for i:= 0 to pred(length) do begin
      result:= TdomASModel(Items[i]).findASNotationDecl(name);
      if assigned(result) then break;
    end;
end;

function TdomASModelCollection.getDomImplementation: TDomImplementation;
begin
  if Assigned(OwnerDocType) then begin
    if Assigned(OwnerDocType.OwnerDocument)
      then Result := OwnerDocType.OwnerDocument.domImplementation
      else Result := nil;
  end else
    Result := nil;
end;

function TdomASModelCollection.getExternalSubset: TdomASModel;
begin
  Result := FASModels[1];
end;

function TdomASModelCollection.getInternalSubset: TdomASModel;
begin
  Result := FASModels[0];
end;

function TdomASModelCollection.getItems(index: integer): TdomASModel;
begin
  result:= TdomASModel(ASModels[index]);
end;

function TdomASModelCollection.getLength: integer;
begin
  result:= ASModels.Count;
end;

function TdomASModelCollection.GetOwnerDocType: TdomDocumentTypeDecl;
begin
  result := GetOwner as TdomDocumentTypeDecl;
end;

function TdomASModelCollection.normalizeAttributeDeclValue(const attrDecl: TdomASAttributeDecl;
                                                             var error: TXmlErrorType): wideString;
// Resolves character references and entity references which are "included in
// literal" (cf. XML 1.0, § 4.4.5).  No complete well-formedness tests are
// performed.
const
  SPACE: WideChar = #$20;  // ' '
var
  entName: wideString;
  i,j,indexpos: integer;
  SChar, SChar2: widechar;
  ref: wideString;
  content: TUtilsCustomWideStr;
  S: wideString;
begin
  error:= ET_NONE;
  content:= TUtilsCustomWideStr.create;
  try
    i:= 1;
    while i <= system.length(attrDecl.defaultValue) do begin
      SChar:= WideChar((PWideChar(attrDecl.defaultValue)+i-1)^);
      if SChar = '&' then begin // Reference?
        indexpos:= -1;
        for j:= i+1 to system.length(attrDecl.defaultValue) do begin
          SChar2:= WideChar((PWideChar(attrDecl.defaultValue)+j-1)^);
          if SChar2 = ';' then begin indexpos:= j; break; end;
        end;
        if indexpos = -1
          then error:= ET_INVALID_ATTRIBUTE_DECL;  // '&' without closing ';'
        ref:= copy(attrDecl.defaultValue, i, indexpos - i + 1);
        if IsXmlEntityRef(ref) then begin
          entName := copy(ref, 2, system.length(ref) - 2);
          error := findASEntityReplacementText(entName, S);
          if error in ET_WARNINGS then begin
            if refersToExternalEntity(entName) then begin
              error:= ET_ATTRIBUTE_VALUE_REFERS_TO_EXTERNAL_ENTITY;
              break;
            end else if pos('<', S) > 0 then begin
              error:= ET_LT_IN_ATTRIBUTE_VALUE;
              break;
            end else content.addWideString(S);
          end else
            break;
        end else if IsXmlCharRef(ref) then begin
          content.addWideString(XmlCharRefToStr(ref));
        end else begin
          error:= ET_INVALID_ATTRIBUTE_DECL;
          break;
        end;
        i:= indexpos;
      end else if IsXmlWhiteSpace(SChar) 
        then content.addWideChar(SPACE)
        else content.addWideChar(SChar);
      inc(i);
    end; {while ...}
    if error = ET_NONE
      then Result:= content.value
      else Result:= '';
  finally
    content.free;
  end;
end;

function TdomASModelCollection.refersToItself(const entDecl: TdomASEntityDecl;
                                              const allowUnresolvableEntities: boolean): boolean;
// This procedure just traverses through all entity references in
// order to test for circular references.  If a circular reference is
// found, the called subroutine 'refersToXyz' raises an EConvertError.
var
  previousEntities: TUtilsWideStringList;
begin
  if not assigned(entDecl)
    then raise ENot_Supported_Err.create('Not supported error.');

  result:= false;
  previousEntities:= TUtilsWideStringList.create;
  try
    previousEntities.add(entDecl.name);
    try
      result:= refersToXyz(entDecl,allowUnresolvableEntities,previousEntities,0);
    except
      raise EConvertError.create('Invalid entity reference error.');
    end;
  finally
    previousEntities.free;
  end;
end;

function TdomASModelCollection.refersToExternalEntity(const entityName: wideString): boolean;
// This function returns 'true', if a node refers directly or
// indirectly to an external Entity.
var
  entDecl: TdomASEntityDecl;
  previousEntities: TUtilsWideStringList;
begin
  entDecl:= findASEntityDecl(entityName);
  if assigned(entDecl) then begin
    result:= false;
    previousEntities:= TUtilsWideStringList.create;
    try
      previousEntities.add(entDecl.name);
      try
        result:= refersToXyz(entDecl,false,previousEntities,1);
      except
        raise EConvertError.create('Invalid entity reference error.');
      end;
    finally
      previousEntities.free;
    end;
  end else if isXmlPredefinedEntityName(entityName) then begin
    result:= false;
  end else raise EConvertError.create('Entity reference cannot be resolved.');
end;

function TdomASModelCollection.refersToLTEntity(const entDecl: TdomASEntityDecl): boolean;
// This function returns 'true', if a node refers directly or indirectly to an
// entity whose replacement text conains a '<' character.
var
  previousEntities: TUtilsWideStringList;
begin
  if not assigned(entDecl)
    then raise ENot_Supported_Err.create('Not supported error.');

  result:= false;
  previousEntities:= TUtilsWideStringList.create;
  try
    previousEntities.add(entDecl.name);
    try
      result:= refersToXyz(entDecl,false,previousEntities,3);
    except
      raise EConvertError.create('Invalid entity reference error.');
    end;
  finally
    previousEntities.free;
  end;
end;

function TdomASModelCollection.refersToUnparsedEntity(const entDecl: TdomASEntityDecl): boolean;
// This function returns 'true', if a node refers directly or
// indirectly to an unparsed Entity.
var
  previousEntities: TUtilsWideStringList;
begin
  if not assigned(entDecl)
    then raise ENot_Supported_Err.create('Not supported error.');

  result:= false;
  previousEntities:= TUtilsWideStringList.create;
  try
    previousEntities.add(entDecl.name);
    try
      result:= refersToXyz(entDecl,true,previousEntities,2);
    except
      raise EConvertError.create('Invalid entity reference error.');
    end;
  finally
    previousEntities.free;
  end;
end;

function TdomASModelCollection.refersToXyz(const entDecl: TdomASEntityDecl;
                                           const allowUnresolvableEntities: boolean;
                                           const previousEntities: TUtilsWideStringList;
                                           const whatToTest: integer): boolean;
// Recursivly tests, whether 'entDecl' is an external entity
// (whatToTest = 1), an unparsed entity (whatToTest = 2), or contains
// a '<' character (whatToTest = 3).
// To just traverse all resolved entity references in order to test
// for circular references, 'whatToTest' must be set to 0.
var
  i: integer;
  dereferencedEntityDecl: TdomASEntityDecl;
  updatedEntities: TUtilsWideStringList;
begin
  with entDecl do begin
    case whatToTest of
      0: result:= false;
      1: result:= entityType = AS_EXTERNAL_ENTITY;
      2: result:= not isParsedEntity;
      3: begin
        if usability = AS_UNRESOLVED then resolve;  // Try to resolve an unresolved entity.
        result:= ContainsLT <> T_FALSE;
      end;
    else
      raise ESyntax_Err.create('Syntax error in TdomASModelCollection.refersToXyz');
    end;
    if result then exit;

    with EntityRefs do
      for i:= 0 to pred(Count) do begin

        if (previousEntities.indexOf(WideStrings[i]) = -1) then begin
          dereferencedEntityDecl:= findASEntityDecl(WideStrings[i]);
          if not assigned(dereferencedEntityDecl) then begin
            if not allowUnresolvableEntities
              then if not isXmlPredefinedEntityName(WideStrings[i])
                then raise EConvertError.create('Entity reference cannot be resolved.');
          end else begin
            updatedEntities:= TUtilsWideStringList.create;
            try
              updatedEntities.Assign(previousEntities);
              updatedEntities.Add(WideStrings[i]);
              Result:= refersToXyz(dereferencedEntityDecl,
                                   allowUnresolvableEntities,
                                   updatedEntities,
                                   whatToTest);
            finally
              updatedEntities.free;
            end;
          end;
        end else begin
          if whatToTest = 0 then
            result:= true;  // circular reference
        end;

        if result then exit;
      end;

  end; {with ...}
end;

function TdomASModelCollection.sendErrorNotification(const xmlErrorType: TXmlErrorType;
                                                     const relASObject: TdomASObject): boolean;
// Used to centralize code for sending error notifications to the DomImplementation.
// Usually used during validation.
var
  error: TdomError;
begin
  error:= TdomError.create(XmlErrorType,-1,-1,-1,-1,-1,-1,-1,-1,'',relASObject,nil,'');
  try
    if assigned(domImplementation) then begin
      result:= domImplementation.handleError(self,error);
    end else if error.severity = DOM_SEVERITY_FATAL_ERROR
      then result:= false
      else result:= true;
  finally
    error.free;
  end;
end;

function TdomASModelCollection.validate: boolean;

  procedure FurtherAttrNormalization(var S: wideString);
  const
    DOUBLESPACE: wideString = #$20#$20;
  var
    nPos: integer;
    dummy: wideString;
  begin
    repeat
      nPos := Pos(DOUBLESPACE, S);
      if nPos > 0 then
        Delete(S, nPos, 1);
    until nPos = 0;
    dummy:= S;
    S:= trimSpace(dummy);
  end;

var
  attrDecl: TdomASAttributeDecl;
  dummy: integer;
  elmtDecl: TdomASElementDecl;
  i: integer;
  j: integer;
  k: integer;
  m: integer;
  idNames: TUtilsWideStringList;
  normalizationError: TXmlErrorType;
  normalizedValue: wideString;
  notationNames: TUtilsWideStringList;
  notationTokens: TUtilsWideStringList;
  enumerationTokens: TUtilsWideStringList;
  enumerationTypes: TUtilsWideStringList;
begin
  result:= true;

  // Validiate Element Declarations:
  enumerationTypes:= TUtilsWideStringList.create;
  enumerationTypes.Sorted:= true;
  enumerationTypes.Duplicates:= dupError;
  try
    with ASModels do
      for i:= 0 to pred(Count) do
        with TdomASModel(Items[i]).elementDecls do
          for j:= 0 to pred(length) do begin
            elmtDecl:= TdomASElementDecl(item(j));
            // VC: No Duplicate Types (XML 1.0, § 3.2.2).
            with elmtDecl do
              if contentType = AS_MIXED_CONTENTTYPE then begin  // xxx Add support for AS_STRICT_MIXED_CONTENTTYPE
                if not assigned(contentModel) then begin
                  result:= false;
                  if not sendErrorNotification(ET_ELEMENT_WITH_ILLEGAL_MIXED_CONTENT,elmtDecl)
                    then exit;
                end else begin
                  with contentModel do
                    if contentModelType = AS_CHOICE_CM then begin
                      enumerationTypes.clear;
                      with subModels do
                        for k:= 0 to pred(length) do begin
                          if item(k).objectType <> AS_CONTENT_MODEL then begin
                            result:= false;
                            if not sendErrorNotification(ET_ELEMENT_WITH_ILLEGAL_MIXED_CONTENT,elmtDecl)
                              then exit;
                          end else begin
                            with TdomASContentModel(item(k)) do
                            if contentModelType <> AS_ELEMENT_CM then begin
                              result:= false;
                              if not sendErrorNotification(ET_ELEMENT_WITH_ILLEGAL_MIXED_CONTENT,elmtDecl)
                                then exit;
                            end else begin
                              if enumerationTypes.find(name,dummy) then begin
                                result:= false;
                                if not sendErrorNotification(ET_DUPLICATE_NAME_IN_MIXED_CONTENT,elmtDecl)
                                  then exit;
                              end else enumerationTypes.add(name);
                            end; {if ... else ...}
                          end; {if ... else ...}
                        end; {for k ...}
                    end else begin
                      result:= false;
                      if not sendErrorNotification(ET_ELEMENT_WITH_ILLEGAL_MIXED_CONTENT,elmtDecl)
                        then exit;
                    end; {if ... else ...}
                end; {if ... else ...}
              end; {if ...}
          end; {for j ...}
  finally
    enumerationTypes.free;
  end;

  // Validiate Entity Declarations:
  with ASModels do
    for i:= 0 to pred(Count) do
      with TdomASModel(Items[i]).entityDecls do
        for j:= 0 to pred(length) do
          with TdomASEntityDecl(item(j)) do
            // VC: Notation Declared (XML 1.0, § 4.2.2)
            if not isParsedEntity
              then if not assigned(findASNotationDecl(notationName)) then begin
                result:= false;
                if not self.sendErrorNotification(ET_UNDECLARED_NOTATION_NAME,TdomASEntityDecl(item(j)))
                  then exit;
              end;

  // Validiate Attribute Declarations:
  enumerationTokens:= TUtilsWideStringList.create;
  enumerationTokens.Sorted:= true;
  enumerationTokens.Duplicates:= dupError;
  idNames:= TUtilsWideStringList.create;
  idNames.Sorted:= true;
  idNames.Duplicates:= dupError;
  notationNames:= TUtilsWideStringList.create;
  notationNames.Sorted:= true;
  notationNames.Duplicates:= dupError;
  notationTokens:= TUtilsWideStringList.create;
  notationTokens.Sorted:= true;
  notationTokens.Duplicates:= dupError;
  try
    with ASModels do
      for i:= 0 to pred(Count) do
        with TdomASModel(Items[i]).elementDecls do
          for j:= 0 to pred(length) do begin
            elmtDecl:= TdomASElementDecl(item(j));
            with elmtDecl.attributeDecls do
              for k:= 0 to pred(length) do begin
                attrDecl:= TdomASAttributeDecl(item(k));
                with attrDecl do
                  case attrType of

                    AS_ID_DATATYPE:
                    begin
                      // VC: One ID per Element Type (XML 1.0, § 3.3.1)
                      if IdNames.find(elmtDecl.name,dummy) then begin
                        result:= false;
                        if not sendErrorNotification(ET_DUPLICATE_ID_ON_ELEMENT_TYPE,TdomASAttributeDecl(item(k)))
                          then exit;
                      end else IdNames.add(elmtDecl.name);
                      // VC: ID Attribute Default (XML 1.0, § 3.3.1)
                      if not ( constraintType in [AVC_IMPLIED, AVC_REQUIRED] ) then begin
                        result:= false;
                        if not sendErrorNotification(ET_ID_NEITHER_IMPLIED_NOR_REQUIRED,TdomASAttributeDecl(item(k)))
                          then exit;
                      end;
                    end;

                    AS_NOTATION_DATATYPE:
                    begin
                      notationTokens.clear;
                      with enumeration do begin
                        for m:= 0 to pred(count) do begin
                          // VC: Notation Attributes (XML 1.0, § 3.3.1)
                          if not assigned(findASNotationDecl(wideStrings[m])) then begin
                            result:= false;
                            if not sendErrorNotification(ET_UNDECLARED_NOTATION_NAME,attrDecl)
                              then exit;
                          end;
                          // VC: No Duplicate Tokens (XML 1.0, 2nd ed., erratum 2)
                          if notationTokens.find(wideStrings[m],dummy) then begin
                            result:= false;
                            if not sendErrorNotification(ET_DUPLICATE_NOTATION_TOKEN,attrDecl)
                              then exit;
                          end else notationTokens.add(wideStrings[m]);
                        end; {for ...}
                      end; {with ...}

                      // VC: One Notation per Element Type (XML 1.0, § 3.3.1)
                      if NotationNames.find(elmtDecl.name,dummy) then begin
                        result:= false;
                        if not sendErrorNotification(ET_DUPLICATE_NOTATION_ON_ELEMENT_TYPE,attrDecl)
                          then exit;
                      end else NotationNames.add(elmtDecl.name);

                      // VC: No Notation on Empty Element (XML 1.0, § 3.3.1)
                      if elmtDecl.contentType = AS_EMPTY_CONTENTTYPE then begin
                        result:= false;
                        if not sendErrorNotification(ET_NOTATION_ON_EMPTY_ELEMENT,attrDecl)
                          then exit;
                      end;
                    end; {AS_NOTATION_DATATYPE ...}

                    AS_IDREF_DATATYPE:
                    begin
                      // VC: IDREF (XML 1.0, § 3.3.1)
                      if constraintType in [AVC_DEFAULT, AVC_FIXED] then begin
                        normalizedValue:= normalizeAttributeDeclValue(attrDecl,normalizationError);
                        FurtherAttrNormalization(normalizedValue);
                        if normalizationError <> ET_NONE then begin
                          result:= false;
                          if not sendErrorNotification(normalizationError,attrDecl)
                             then exit;
                        end;
                        if not isXMLName(normalizedValue) then begin
                          result:= false;
                          if not sendErrorNotification(ET_ATTRIBUTE_DEFAULT_TYPE_MISMATCH,attrDecl)
                             then exit;
                        end;
                      end;
                    end;

                    AS_IDREFS_DATATYPE:
                    begin
                      // VC: IDREF (XML 1.0, § 3.3.1)
                      if constraintType in [AVC_DEFAULT, AVC_FIXED] then begin
                        normalizedValue:= normalizeAttributeDeclValue(attrDecl,normalizationError);
                        FurtherAttrNormalization(normalizedValue);
                        if normalizationError <> ET_NONE then begin
                          result:= false;
                          if not sendErrorNotification(normalizationError,attrDecl)
                             then exit;
                        end;
                        if not isXMLNames(normalizedValue) then begin
                          result:= false;
                          if not sendErrorNotification(ET_ATTRIBUTE_DEFAULT_TYPE_MISMATCH,attrDecl)
                             then exit;
                        end;
                      end;
                    end;

                    AS_ENTITY_DATATYPE:
                    begin
                      // VC: Entity Name (XML 1.0, § 3.3.1)
                      if constraintType in [AVC_DEFAULT, AVC_FIXED] then begin
                        normalizedValue:= normalizeAttributeDeclValue(attrDecl,normalizationError);
                        FurtherAttrNormalization(normalizedValue);
                        if normalizationError <> ET_NONE then begin
                          result:= false;
                          if not sendErrorNotification(normalizationError,attrDecl)
                             then exit;
                        end;
                        if not isXMLName(normalizedValue) then begin
                          result:= false;
                          if not sendErrorNotification(ET_ATTRIBUTE_DEFAULT_TYPE_MISMATCH,attrDecl)
                             then exit;
                        end;
                      end;
                    end;

                    AS_ENTITIES_DATATYPE:
                    begin
                      // VC: Entity Name (XML 1.0, § 3.3.1)
                      if constraintType in [AVC_DEFAULT, AVC_FIXED] then begin
                        normalizedValue:= normalizeAttributeDeclValue(attrDecl,normalizationError);
                        FurtherAttrNormalization(normalizedValue);
                        if normalizationError <> ET_NONE then begin
                          result:= false;
                          if not sendErrorNotification(normalizationError,attrDecl)
                             then exit;
                        end;
                        if not isXMLNames(normalizedValue) then begin
                          result:= false;
                          if not sendErrorNotification(ET_ATTRIBUTE_DEFAULT_TYPE_MISMATCH,attrDecl)
                             then exit;
                        end;
                      end;
                    end;

                    AS_NMTOKEN_DATATYPE:
                    begin
                      // VC: name Token (XML 1.0, § 3.3.1)
                      if constraintType in [AVC_DEFAULT, AVC_FIXED] then begin
                        normalizedValue:= normalizeAttributeDeclValue(attrDecl,normalizationError);
                        FurtherAttrNormalization(normalizedValue);
                        if normalizationError <> ET_NONE then begin
                          result:= false;
                          if not sendErrorNotification(normalizationError,attrDecl)
                             then exit;
                        end;
                        if not isXmlNmtoken(normalizedValue) then begin
                          result:= false;
                          if not sendErrorNotification(ET_ATTRIBUTE_DEFAULT_TYPE_MISMATCH,attrDecl)
                             then exit;
                        end;
                      end;

                      // VC: No Duplicate Tokens (XML 1.0, 2nd ed., erratum 2)
                      EnumerationTokens.clear;
                      with enumeration do begin
                        for m:= 0 to pred(count) do begin
                          if EnumerationTokens.find(wideStrings[m],dummy) then begin
                            result:= false;
                            if not sendErrorNotification(ET_DUPLICATE_ENUMERATION_TOKEN,attrDecl)
                              then exit;
                          end else EnumerationTokens.add(wideStrings[m]);
                        end; {for ...}
                      end; {with ...}
                    end;

                    AS_NMTOKENS_DATATYPE:
                    begin
                      // VC: name Token (XML 1.0, § 3.3.1)
                      if constraintType in [AVC_DEFAULT, AVC_FIXED] then begin
                        normalizedValue:= normalizeAttributeDeclValue(attrDecl,normalizationError);
                        FurtherAttrNormalization(normalizedValue);
                        if normalizationError <> ET_NONE then begin
                          result:= false;
                          if not sendErrorNotification(normalizationError,attrDecl)
                             then exit;
                        end;
                        if not isXmlNmtokens(normalizedValue) then begin
                          result:= false;
                          if not sendErrorNotification(ET_ATTRIBUTE_DEFAULT_TYPE_MISMATCH,attrDecl)
                             then exit;
                        end;
                      end;
                    end;

                  end; {case}
            end; {for k ...}
          end; {for j ...}
  finally
    enumerationTokens.free;
    idNames.free;
    notationNames.free;
    notationTokens.free;
  end;
end;



{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
//+++++++++++++++++++++ TdomASModelCollectionNS +++++++++++++++++++++++++
constructor TdomASModelCollectionNS.create(const aOwner: TdomDocument);
begin
  if not assigned(aOwner)
    then raise ENot_Supported_Err.create('Not supported error.');
  inherited create;
  FASModelsNS:= TList.Create;
  FOwnerDocument:= aOwner;
end;

destructor TdomASModelCollectionNS.destroy;
begin
  clear;
  FASModelsNS.Free;
  inherited;
end;

function TdomASModelCollectionNS.add(const model: TdomASModelNS): integer;
begin
  if not assigned(model)
    then raise ENot_Supported_Err.create('Not supported error.');
  if model.domImplementation <> ownerDocument.domImplementation
    then raise EWrong_DOM_Implementation_Err.create('Wrong DOM implementation error.');
  if FASModelsNS.indexOf(model) > -1
    then raise EInuse_Err.create('Inuse ASModel error.');
  result:= FASModelsNS.Add(model);
  model.ownerCollections.Add(self);
end;

procedure TdomASModelCollectionNS.clear;
var
  i: integer;
begin
  with FASModelsNS do begin
    for i:= 0 to pred(count) do
      TdomCustomASModelNS(Items[i]).ownerCollections.remove(self);
    clear;
  end;
end;

function TdomASModelCollectionNS.findASAttributeDecl(const elementNamespaceURI,
                                                           elementLocalName,
                                                           attributeNamespaceURI,
                                                           attributeLocalName: wideString): TdomASAttributeDeclNS;
var
  i: integer;
begin
  result:= nil;
  with FASModelsNS do
    for i:= 0 to pred(length) do begin
      result:= TdomASModelNS(Items[i]).findASAttributeDecl(elementNamespaceURI,elementLocalName,attributeNamespaceURI,attributeLocalName);
      if assigned(result) then break;
    end;
end;

function TdomASModelCollectionNS.findASElementDecl(const namespaceURI,
                                                         localName: wideString): TdomASElementDeclNS;
var
  i: integer;
begin
  result:= nil;
  with FASModelsNS do
    for i:= 0 to pred(length) do begin
      result:= TdomASModelNS(Items[i]).findASElementDecl(namespaceURI,localname);
      if assigned(result) then break;
    end;
end;

function TdomASModelCollectionNS.findASEntityDecl(const namespaceURI,
                                                        localName: wideString): TdomASEntityDeclNS;
var
  i: integer;
begin
  result:= nil;
  with FASModelsNS do
    for i:= 0 to pred(length) do begin
      result:= TdomASModelNS(Items[i]).findASEntityDecl(namespaceURI,localname);
      if assigned(result) then break;
    end;
end;

function TdomASModelCollectionNS.findASNotationDecl(const namespaceURI,
                                                          localName: wideString): TdomASNotationDeclNS;
var
  i: integer;
begin
  result:= nil;
  with FASModelsNS do
    for i:= 0 to pred(length) do begin
      result:= TdomASModelNS(Items[i]).findASNotationDecl(namespaceURI,localname);
      if assigned(result) then break;
    end;
end;

function TdomASModelCollectionNS.getItems(index: integer): TdomASModelNS;
begin
  result:= TdomASModelNS(FASModelsNS[index]);
end;

function TdomASModelCollectionNS.getLength: integer;
begin
  result:= FASModelsNS.Count;
end;

procedure TdomASModelCollectionNS.insert(const index: integer;
                                               model: TdomASModelNS);
begin
  if not assigned(model)
    then raise ENot_Supported_Err.create('Not supported error.');
  if model.domImplementation <> ownerDocument.domImplementation
    then raise EWrong_DOM_Implementation_Err.create('Wrong DOM implementation error.');
  if FASModelsNS.indexOf(model) > -1 
    then raise EInuse_Err.create('Inuse ASModel error.');
  FASModelsNS.insert(index,model);
  model.ownerCollections.Add(self);
end;

function TdomASModelCollectionNS.remove(const model: TdomASModelNS): integer;
begin
  if not assigned(model)
    then raise ENot_Supported_Err.create('Not supported error.');
  result:= FASModelsNS.remove(model);
  model.ownerCollections.remove(self);
end;
{$endif}



//+++++++++++++++++++++++++++ TdomASObject +++++++++++++++++++++++++++++++
constructor TdomASObject.create(const aOwner: TdomASModel;
                                const aName: wideString);
begin
  inherited create;
  FObjectType:= AS_UNDEFINED;
  FOwnerModel:= aOwner;
  FName:= aName;
end;

function TdomASObject.getName: wideString;
begin
  result:= FName;
end;



{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
//+++++++++++++++++++++++ TdomCustomASObjectNS +++++++++++++++++++++++++++
constructor TdomCustomASObjectNS.create(const aOwner: TdomCustomASModelNS);
begin
  inherited create;
  FObjectType:= AS_UNDEFINED;
  FOwnerModel:= aOwner;
end;



//++++++++++++++++++++++++++ TdomASObjectNS ++++++++++++++++++++++++++++++
constructor TdomASObjectNS.create(const aOwner: TdomASModelNS;
                                  const aNamespaceURI,
                                        aPrefix,
                                        aLocalName: wideString);
begin
  if (aNamespaceURI <> '') or (aPrefix <> '') or (aLocalName <> '') then begin
    if not isXmlLocalPart(aLocalName)
      then raise EInvalid_Character_Err.create('Invalid character error.');
    if aPrefix <> '' then begin
      if aNamespaceURI = ''
        then raise ENamespace_Err.create('Namespace error.');
      if not isXmlPrefix(aPrefix)
        then raise ENamespace_Err.create('Namespace error.');
      if (aPrefix = 'xmlns') and (aNamespaceURI <> 'http://www.w3.org/2000/xmlns/')
          then raise ENamespace_Err.create('Namespace error.');
      if (aPrefix = 'xml') and (aNamespaceURI <> 'http://www.w3.org/XML/1998/namespace')
        then raise ENamespace_Err.create('Namespace error.');
    end;
    if (aLocalName = 'xmlns') and (aNamespaceURI <> 'http://www.w3.org/2000/xmlns/')
      then raise ENamespace_Err.create('Namespace error.');
  end;
  inherited create(aOwner);
  FNameSpaceURI:= aNamespaceURI;
  FPrefix:= aPrefix;
  FLocalName:= aLocalName;
  FIsNamespaceAware:= true;
end;

function TdomASObjectNS.getName: wideString;
begin
  if FPrefix = ''
    then result:= FLocalName
    else result:= concat(FPrefix,':',FLocalName);
end;
{$endif}



//+++++++++++++++++++++++++++ TdomASContentModel +++++++++++++++++++++++++++
constructor TdomASContentModel.create(const aOwnerElementDecl: TdomASElementDecl;
                                      const aName: wideString;
                                      const aContentModelType: TdomASContentModelType);
begin
  if aContentModelType = AS_ELEMENT_CM
    then inherited create(aOwnerElementDecl.ownerModel as TdomASModel,aName)
    else inherited create(aOwnerElementDecl.ownerModel as TdomASModel,'');

  case aContentModelType of

{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
    AS_ALL_CM:
      raise ENot_Supported_Err.create('Not supported error.');
{$endif}

    AS_ELEMENT_CM:
      FAllowedChildTypes:= [];

    AS_CHOICE_CM, AS_SEQUENCE_CM:
      FAllowedChildTypes:= [AS_ELEMENT_CM,
                            AS_CHOICE_CM,
                            AS_SEQUENCE_CM];

  end;

  FContentModelType:= aContentModelType;
  FFrequency:= AS_REQUIRED_FRQ;
  FInuse:= false;
  FObjectType:= AS_CONTENT_MODEL;
  FOwnerElementDecl:= aOwnerElementDecl;
  FSubModels:= TdomASObjectList.create;
end;

destructor TdomASContentModel.destroy;
begin
  FSubModels.free;
  inherited;
end;

function TdomASContentModel.appendSubModel(const newCM: TdomASContentModel): TdomASContentModel;
begin
  if newCM.FInuse
    then raise EInuse_Err.create('Content model in use error.');
  result:= (FSubModels.appendASNode(newCM) as TdomASContentModel);
  newCM.FInuse:= true;
end;

function TdomASContentModel.insertBeforeSubModel(const newCM,
                                                       refCM: TdomASContentModel): TdomASContentModel;
begin
  if newCM.FInuse
    then raise EInuse_Err.create('Content model in use error.');
  result:= (FSubModels.insertBefore(newCM,refCM) as TdomASContentModel);
  newCM.FInuse:= true;
end;

function TdomASContentModel.removeSubModel(const oldCM: TdomASContentModel): TdomASContentModel;
begin
  if FSubModels.indexof(oldCM) = -1
    then raise ENot_Found_Err.create('Node not found error.');
  result:= (FSubModels.removeASNode(oldCM) as TdomASContentModel);
  oldCM.FInuse:= false;
end;

function TdomASContentModel.validateChoiceNames(const source: TUtilsWideStringList;
                                                  var index: integer;
                                                      freq: TdomASFrequency;
                                                  out isNonDeterministic: boolean): boolean;
var
  i: integer;
  matched: boolean;
  matchNumber: integer;
  restindex, tempindex: integer;
begin
  isNonDeterministic:= false;
  result:= false;

  restindex:= index;
  matched:= False;
  matchNumber:= 0;
  for i:= 0 to pred(subModels.length) do begin
    tempindex:= index;
    if (subModels.item(i) as TdomASContentModel).validateNames(source,tempindex,isNonDeterministic) then begin
      matched := True;
      if index <> tempindex then begin // Do not count matching empty expressions
        inc(matchNumber);
        if matchNumber > 1 then begin
          isNonDeterministic:= true;
          break;
        end;
        restindex:= tempindex;
      end;
    end else if isNonDeterministic then break;
  end;

  case freq of

    AS_REQUIRED_FRQ:
    begin
      if matched then begin
        index:= restindex;
        result:= true;
      end else result:= false;
    end;

    AS_OPTIONAL_FRQ:
    begin
      if matched then index:= restindex;
      result:= true;
    end;

  end; {case ...}

  if isNonDeterministic then result:= false;
end;

function TdomASContentModel.validateElementNames(const source: TUtilsWideStringList;
                                                   var index: integer;
                                                       freq: TdomASFrequency;
                                                   out isNonDeterministic: boolean): boolean;
begin
  isNonDeterministic:= false;
  result:= false;

  case freq of

    AS_REQUIRED_FRQ:
    begin
      if index = source.count then exit;
      if source[index] = name then begin
        inc(index);
        result:= true;
      end else result:= false;
    end;

    AS_OPTIONAL_FRQ:
    begin
      result:= true;
      if index = source.count then exit;
      if source[index] = name then Inc(index);
    end;

  end; {case ...}
end;

function TdomASContentModel.validateNames2(const source: TUtilsWideStringList;
                                             var index: integer;
                                                 freq: TdomASFrequency;
                                             out isNonDeterministic: boolean): boolean;
begin
  case contentModelType of
    AS_CHOICE_CM:   result:= validateChoiceNames(source,index,freq,isNonDeterministic);
    AS_ELEMENT_CM:  result:= validateElementNames(source,index,freq,isNonDeterministic);
    AS_SEQUENCE_CM: result:= validateSequenceNames(source,index,freq,isNonDeterministic);
  else
    result:= true;
  end;
end;

function TdomASContentModel.validateNames(const source: TUtilsWideStringList;
                                            var index: integer; 
                                            out isNonDeterministic: boolean): boolean;
// Validates a sequence of names contained in the 'source' list, starting at
// the 'index' position against the content model.  If successful, 'index'
// returns the position of the first name of the 'source' list which remains
// after applying the content model to the names of the list.
var
  tempindex: integer; 
begin
  result:= false;
  isNonDeterministic:= false;
  case Frequency of

    AS_REQUIRED_FRQ:
      result:= validateNames2(source,index,AS_REQUIRED_FRQ,isNonDeterministic);

    AS_OPTIONAL_FRQ:
    result:= validateNames2(source,index,AS_OPTIONAL_FRQ,isNonDeterministic);

    AS_ONE_OR_MORE_FRQ:
    begin
      result:= validateNames2(source,index,AS_REQUIRED_FRQ,isNonDeterministic);
      if result then begin
        tempindex := index;
        while tempindex < source.count do begin
          if not validateNames2(source,tempindex,AS_REQUIRED_FRQ,isNonDeterministic) then break;
          if index = tempindex then break; // Test for expressions of the form: (foo*)+
          index := tempindex;
        end;
      end;
    end;

    AS_ZERO_OR_MORE_FRQ:
    begin
      result:= validateNames2(source,index,AS_OPTIONAL_FRQ,isNonDeterministic);
      if result then begin
        tempindex := index;
        while tempindex < source.count do begin
          if not validateNames2(source,tempindex,AS_REQUIRED_FRQ,isNonDeterministic) then break;
          if index = tempindex then break; // Test for expressions of the form: (foo*)* or (foo+)*
          index := tempindex;
        end;
      end;
    end;

  end; {case ...}

  if isNonDeterministic then result:= false;
end;

function TdomASContentModel.validateSequenceNames(const source: TUtilsWideStringList;
                                                    var index: integer;
                                                        freq: TdomASFrequency;
                                                    out isNonDeterministic: boolean): boolean;
var
  i: integer;
  ok: boolean;
  tempindex: integer;
begin
  isNonDeterministic:= false;
  result:= false;
  tempindex:= index;

  ok:= false;
  for i:= 0 to pred(subModels.length) do begin
    ok:= (subModels.item(i) as TdomASContentModel).validateNames(source,tempindex,isNonDeterministic);
    if not ok then break;
  end;

  case freq of

    AS_REQUIRED_FRQ:
    begin
      if ok then begin
        index := tempindex;
        result:= true;
      end else result:= false;
    end;

    AS_OPTIONAL_FRQ:
    begin
      if ok then index := tempindex;
      result:= true;
    end;

  end; {case ...}

  if isNonDeterministic then result:= false;
end;



{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
//++++++++++++++++++++++++++ TdomASContentModelNS ++++++++++++++++++++++++++
constructor TdomASContentModelNS.create(const aOwnerElementDecl: TdomASElementDeclNS;
                                        const aContentModelType: TdomASContentModelType);
begin
  inherited create(aOwnerElementDecl.ownerModel as TdomASModelNS,'','','');

  case aContentModelType of

    AS_ALL_CM:
      FAllowedChildTypes:= [AS_ELEMENT_CM];

    AS_ELEMENT_CM:
      FAllowedChildTypes:= [];

    AS_CHOICE_CM, AS_SEQUENCE_CM:
      FAllowedChildTypes:= [AS_ELEMENT_CM,
                            AS_CHOICE_CM,
                            AS_SEQUENCE_CM];

  end;

  FContentModelType:= aContentModelType;
  FInuse:= false;
  FMaxOccurs:= 1;
  FMinOccurs:= 1;
  FObjectType:= AS_CONTENT_MODEL;
  FOwnerElementDecl:= aOwnerElementDecl;
  FSubModels:= TdomASObjectListNS.create;
end;

destructor TdomASContentModelNS.destroy;
begin
  FSubModels.free;
  inherited;
end;

function TdomASContentModelNS.appendSubModel(const newCM: TdomASContentModelNS): TdomASContentModelNS;
begin
  if newCM.FInuse
    then raise EInuse_Err.create('Content model in use error.');
  result:= (FSubModels.appendASNode(newCM) as TdomASContentModelNS);
  newCM.FInuse:= true;
end;

function TdomASContentModelNS.getName: wideString;
begin
  result:= '';
end;

function TdomASContentModelNS.insertBeforeSubModel(const newCM,
                                                         refCM: TdomASContentModelNS): TdomASContentModelNS;
begin
  if newCM.FInuse
    then raise EInuse_Err.create('Content model in use error.');
  result:= (FSubModels.insertBefore(newCM,refCM) as TdomASContentModelNS);
  newCM.FInuse:= true;
end;

function TdomASContentModelNS.removeSubModel(const oldCM: TdomASContentModelNS): TdomASContentModelNS;
begin
  if FSubModels.indexof(oldCM) = -1
    then raise ENot_Found_Err.create('Node not found error.');
  result:= (FSubModels.removeASNode(oldCM) as TdomASContentModelNS);
  oldCM.FInuse:= false;
end;

procedure TdomASContentModelNS.setMaxOccurs(const value: integer);
begin
  if (value < 0) or ( (contentModelType = AS_ALL_CM) and (value > 1) )
    then raise ENot_Supported_Err.create('Not supported error.');
  FMaxOccurs:= value;
end;

procedure TdomASContentModelNS.setMinOccurs(const value: integer);
begin
  if (value < 0) or ( (contentModelType = AS_ALL_CM) and (value > 1) )
    then raise ENot_Supported_Err.create('Not supported error.');
  FMinOccurs:= value;
end;
{$endif}



//+++++++++++++++++++++++++ TdomASAttributeDecl ++++++++++++++++++++++++++
constructor TdomASAttributeDecl.create(const aOwnerElementDecl: TdomASElementDecl;
                                       const aAttrName,
                                             aDefaultValue: wideString;
                                       const aEnumeration: TUtilsWideStringList;
                                       const aAttrType: TXmlDataType;
                                       const aConstraintType: TdomAttrValueConstraint);
begin
  if not IsXmlName(aAttrName) then
    raise EInvalid_Character_Err.create('Invalid character error.');
  inherited create(aOwnerElementDecl.ownerModel as TdomASModel, aAttrName);
  FObjectType:= AS_ATTRIBUTE_DECLARATION;
  FAttrType:= aAttrType;
  FDefaultValue:= aDefaultValue;
  FConstraintType:= aConstraintType;
  FOwnerElementDecl:= aOwnerElementDecl;
  FEnumeration:= TUtilsWideStringList.create;
  FEnumeration.Assign(aEnumeration);
end;

destructor TdomASAttributeDecl.destroy;
begin
  FEnumeration.free;
  inherited;
end;



{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
//++++++++++++++++++++++++ TdomASAttributeDeclNS +++++++++++++++++++++++++
constructor TdomASAttributeDeclNS.create(const aOwnerElementDecl: TdomASElementDeclNS;
                                         const aNamespaceURI,
                                               aPrefix,
                                               aLocalName: wideString);
begin
  if aLocalName = ''
    then raise EInvalid_Character_Err.create('Invalid character error.');
  inherited create(aOwnerElementDecl.ownerModel as TdomASModelNS,aNamespaceURI,aPrefix,aLocalName);
  FObjectType:= AS_ATTRIBUTE_DECLARATION;
  FAttrType:= AS_STRING_DATATYPE;
  FAttrValue:= '';
  FConstraintType:= AVC_IMPLIED;
  FEnumAttr:= TUtilsWideStringList.create;
  FOwnerElementDecl:= aOwnerElementDecl;
end;

destructor TdomASAttributeDeclNS.destroy;
begin
  FEnumAttr.free;
  inherited;
end;
{$endif}



//+++++++++++++++++++++++++++ TdomASEntityDecl +++++++++++++++++++++++++++
constructor TdomASEntityDecl.create(const aOwner: TdomASModel;
                                    const aName,
                                          aReplacementText,
                                          aPublicId,
                                          aSystemId,
                                          aNotationName: wideString);
begin
  if not IsXmlName(aName)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  inherited create(aOwner,aName);

  FObjectType:= AS_ENTITY_DECLARATION;
  FPublicId:= aPublicId;
  FSystemId:= aSystemId;
  FNotationName:= aNotationName;

  FEntityRefs:= TUtilsWideStringList.Create;
  FEntityRefs.Duplicates:= dupIgnore;

  if entityType = AS_INTERNAL_ENTITY then begin
    setReplacementText(aReplacementText);
  end else begin
    FReplacementText:= '';
    FUsability:= AS_UNRESOLVED;
    FContainsLT:= T_UNKNOWN;
  end;
end;

destructor TdomASEntityDecl.destroy;
begin
  FEntityRefs.Free;
  inherited;
end;

function TdomASEntityDecl.getEntityType: TdomASEntityType;
begin
  if (FPublicId = '') and (FSystemId = '') and (FNotationName = '')
    then result:= AS_INTERNAL_ENTITY
    else result:= AS_EXTERNAL_ENTITY;
end;

function TdomASEntityDecl.getIsParsedEntity: boolean;
begin
  result:= FNotationName = '';
end;

function TdomASEntityDecl.resolve: boolean;
var
  Content: TUtilsCustomWideStr;
  InputSrc: TXmlInputSource;
  PId: WideString;
  SId: WideString;
  Stream: TStream;
begin
  if FUsability = AS_UNRESOLVED then begin
    Result := False;
    if isParsedEntity and (entityType = AS_EXTERNAL_ENTITY) then begin
      PId := publicId;
      SId := systemId;
      stream:= ownerModel.domImplementation.resolveResource(ownerModel.location,PId,SId);
      if Assigned(Stream) then begin
        try
          // convert external entity value to UTF-16:
          InputSrc:= TXmlInputSource.create(Stream, PId, SId, 4096, 'UTF-8',
                       False, 0, 0, 0, 0, 1);  // xxx implement default encoding?  xxx Change offsetFromBeginning parameter?
          try
            with InputSrc do begin
              if hasMalformedDecl
               or invalidEncoding
               or not ( declType in [ DT_TEXT_DECLARATION,
                                      DT_XML_OR_TEXT_DECLARATION,
                                      DT_UNSPECIFIED ] )
              then result:= false
              else begin
                Content := TUtilsCustomWideStr.create;
                try
                  Next;
                  while not Eof do
                  begin
                    Content.AddUCS4Char(CurrentCodePoint);
                    Next;
                  end;
                  SetReplacementText(Content.value);
                  Result := True;
                finally
                  Content.Free;
                end;
              end; {if ... else ...}
            end; {with ...}
          finally
            InputSrc.Free;
          end; {try}
        finally
          Stream.Free;
        end;
      end; {if ...}

    end; {if ...}
  end else Result := True;
end;

procedure TdomASEntityDecl.setReplacementText(const S: wideString);
var
  Parser: TXmlToDomParser;
  Doc: TdomDocument;
  EntRef: TdomNode;
  EntRefIterator: TdomNodeIterator;
  LTIterator: TdomNodeIterator;
  OnErrorBackup: TdomErrorEvent;
begin
  FReplacementText := S;
  FUsability := AS_USABLE;
  FContainsLT := T_FALSE;
  FEntityRefs.Clear;
  if FReplacementText <> '' then begin
    Parser := TXmlToDomParser.Create(nil);
    try
      Parser.DomImpl := OwnerModel.DomImplementation;
      Doc := OwnerModel.DomImplementation.CreateDocument('dummy', nil);
      try
        OnErrorBackup := OwnerModel.DomImplementation.OnError;
        OwnerModel.DomImplementation.OnError := nil;  // Suppress error reports.
        try
          Parser.ParseWideString(FReplacementText, '', '', Doc.DocumentElement);

          EntRefIterator := Doc.CreateNodeIterator(Doc.DocumentElement, [ntEntity_Reference_Node], nil, False);
          EntRef := EntRefIterator.NextNode;
          while Assigned(EntRef) do begin
            FEntityRefs.Add(EntRef.NodeName);
            EntRef := EntRefIterator.NextNode;
          end;

          LTIterator := Doc.CreateNodeIterator(
                          Doc.DocumentElement,
                          [ ntElement_Node,
                            ntCDATA_Section_Node,
                            ntProcessing_Instruction_Node,
                            ntComment_Node ],
                          nil,
                          False);
          LTIterator.NextNode; // Remark: Returns the <dummy> root element.
          if LTIterator.NextNode <> nil then
            FContainsLT := T_TRUE;
        finally
          OwnerModel.DomImplementation.OnError := OnErrorBackup;
        end;
      except
        FUsability := AS_UNUSABLE;
      end; {try ...}
      OwnerModel.DomImplementation.FreeDocument(Doc);
    finally
      Parser.Free;
    end; {try ...}
  end; {if ...}
end;



{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
//++++++++++++++++++++++++++ TdomASEntityDeclNS ++++++++++++++++++++++++++
constructor TdomASEntityDeclNS.create(const aOwner: TdomASModelNS;
                                      const aNamespaceURI,
                                            aPrefix,
                                            aLocalName,
                                            aReplacementText,
                                            aPublicId,
                                            aSystemId,
                                            aNotationName: wideString);
begin
  if aLocalName = ''
    then raise EInvalid_Character_Err.create('Invalid character error.');
  inherited create(aOwner,aNamespaceURI,aPrefix,aLocalName);

  FObjectType:= AS_ENTITY_DECLARATION;
  FPublicId:= aPublicId;
  FSystemId:= aSystemId;
  FNotationName:= aNotationName;

  if entityType = AS_INTERNAL_ENTITY then begin
    setReplacementText(aReplacementText);
    FUsability:= establishUsability(FReplacementText);
  end else begin
    FReplacementText:= '';
    FUsability:= AS_UNRESOLVED;
  end;
end;

function TdomASEntityDeclNS.establishUsability(const S: wideString): TdomASEntityUsability;
var
  parser: TXmlToDomParser;
  dummyDoc: TdomDocument;
  OnErrorBackup: TdomErrorEvent;
begin
  result:= AS_USABLE;
  if S <> '' then begin
    parser:= TXmlToDomParser.create(nil);
    try
      parser.domImpl:= ownerModel.domImplementation;
      dummyDoc:= ownerModel.domImplementation.createDocument('dummy',nil);
      try
        OnErrorBackup:= ownerModel.domImplementation.OnError;
        ownerModel.domImplementation.OnError := nil;  // Suppress error reports.
        try
          parser.parseWideString(S,'','',dummyDoc.documentElement);
        finally
          ownerModel.domImplementation.OnError:= OnErrorBackup;
        end;
      except
        result:= AS_UNUSABLE;
      end; {try ...}
      ownerModel.domImplementation.FreeDocument(dummyDoc);
    finally
      parser.free;
    end; {try ...}
  end; {if ...}
end;

function TdomASEntityDeclNS.getEntityType: TdomASEntityType;
begin
  if (FPublicId = '') and (FSystemId = '')
    then result:= AS_INTERNAL_ENTITY
    else result:= AS_EXTERNAL_ENTITY;
end;

function TdomASEntityDeclNS.getIsParsedEntity: boolean;
begin
  result:= FNotationName = '';
end;

procedure TdomASEntityDeclNS.setReplacementText(const S: wideString);
begin
  FReplacementText:= S;
end;
{$endif}



//++++++++++++++++++++++++++ TdomASNotationDecl ++++++++++++++++++++++++++
constructor TdomASNotationDecl.create(const aOwner: TdomASModel;
                                      const aName,
                                            aPublicId,
                                            aSystemId: wideString);
begin
  if not IsXmlName(aName)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  if not isXmlPubidChars(aPublicId)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  if not isXmlSystemChars(aSystemId)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  inherited create(aOwner,aName);
  FObjectType:= AS_NOTATION_DECLARATION;
  FPublicId:= aPublicId;
  FSystemId:= aSystemId;
end;



{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
//+++++++++++++++++++++++++ TdomASNotationDeclNS +++++++++++++++++++++++++
constructor TdomASNotationDeclNS.create(const aOwner: TdomASModelNS;
                                        const aNamespaceURI,
                                              aPrefix,
                                              aLocalName,
                                              aPublicId,
                                              aSystemId: wideString);
begin
  if aLocalName = ''
    then raise EInvalid_Character_Err.create('Invalid character error.');
  if not isXmlPubidChars(aPublicId)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  if not isXmlSystemChars(aSystemId)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  inherited create(aOwner,aNamespaceURI,aPrefix,aLocalName);
  FObjectType:= AS_NOTATION_DECLARATION;
  FPublicId:= aPublicId;
  FSystemId:= aSystemId;
end;
{$endif}



//++++++++++++++++++++++++++ TdomASElementDecl +++++++++++++++++++++++++++
constructor TdomASElementDecl.create(const aOwner: TdomASModel;
                                     const aName: wideString;
                                     const aContentType: TdomASContentType);
begin
  if not IsXmlName(aName)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  inherited create(aOwner,aName);

  FContentType:= AS_UNKNOWN_CONTENTTYPE;  // First set FContentType to AS_UNKNOWN_CONTENTTYPE, ...
  setContentType(aContentType);           // ... so setContentType() does not raise an exception.

  FAttributeDeclarations:= TdomASNamedObjectMap.create(aOwner);
  FCreatedContentModels:= TdomASObjectList.create;
  FObjectType:= AS_ELEMENT_DECLARATION;
end;

destructor TdomASElementDecl.destroy;
begin
  clear;
  FAttributeDeclarations.free;
  FCreatedContentModels.free;
  inherited;
end;

procedure TdomASElementDecl.clear;
var
  i: integer;
begin
  for i:= 0 to pred(FAttributeDeclarations.length) do
    TdomASAttributeDecl(FAttributeDeclarations.item(i)).free;
  FAttributeDeclarations.clear;
  for i:= 0 to pred(FCreatedContentModels.length) do
    TdomASContentModel(FCreatedContentModels.item(i)).free;
  FCreatedContentModels.clear;
  FContentModel:= nil;
end;

function TdomASElementDecl.createContentModel(const name: wideString;
                                              const contentModelType: TdomASContentModelType): TdomASContentModel;
begin
  result:= TdomASContentModel.create(self,name,contentModelType);
  FCreatedContentModels.appendASNode(result);
end;

function TdomASElementDecl.findASAttributeDecl(const name: wideString): TdomASAttributeDecl;
begin
  result:= (FAttributeDeclarations.getNamedItem(name) as TdomASAttributeDecl);
end;

procedure TdomASElementDecl.freeAndNilContentModel(var cm: TdomASContentModel);
var
  submdl: TdomASContentModel;
begin
  if cm.FInuse
    then raise EInuse_Err.create('Content model in use error.');
  // First recursively free the submodels:
  with cm do
    with subModels do
      while length > 0 do begin
        submdl:= removeSubModel(item(pred(length)) as TdomASContentModel);
        freeAndNilContentModel(submdl);
      end;
  // Now free the content model:
  FCreatedContentModels.removeASNode(cm);
  cm.free;
  cm:= nil;
end;

function TdomASElementDecl.removeASAttributeDecl(const name: wideString): boolean;
var
  obj: TdomASObject;
begin
  obj:= FAttributeDeclarations.getNamedItem(name);
  if assigned(obj) then begin
    FAttributeDeclarations.removeNamedItem(name);
    obj.Free;
    result:= true;
  end else result:= false;
end;

function TdomASElementDecl.replaceContentModel(const newContentModel: TdomASContentModel): TdomASContentModel;
begin
  if FContentModel = newContentModel then begin
    result:= newContentModel;
    exit;
  end;
  if assigned(newContentModel) then begin
    if newContentModel.ownerModel <> ownerModel
      then raise EAS_Wrong_Element_Decl_Err.create('Wrong element declaration error.');
    if not (newContentModel.contentModelType in FAllowedChildTypes)
      then raise ENot_Supported_Err.create('Not supported error.');
    if newContentModel.FInuse
      then raise EInuse_Err.create('Content model in use error.');
    newContentModel.FInuse:= true;
  end;
  if assigned(FContentModel)
    then FContentModel.FInuse:= false;
  result:= FContentModel;
  FContentModel:= newContentModel;
end;

function TdomASElementDecl.setASAttributeDecl(const aAttrName,
                                                    aAttrValue: wideString;
                                              const aEnumeration: TUtilsWideStringList;
                                              const aAttrType: TXmlDataType;
                                              const aConstraintType: TdomAttrValueConstraint;
                                                out attributeDecl: TdomASAttributeDecl): boolean;
begin
  attributeDecl:= findASAttributeDecl(aAttrName);
  if assigned(attributeDecl) then begin
    result:= false;
  end else begin
    attributeDecl:= TdomASAttributeDecl.create(self, aAttrName, aAttrValue, aEnumeration, aAttrType, aConstraintType);
    FAttributeDeclarations.setNamedItem(attributeDecl);
    result:= true;
  end;
end;

procedure TdomASElementDecl.setContentType(const value: TdomASContentType);
begin
  if FContentType <> AS_UNKNOWN_CONTENTTYPE
    then raise ENo_Modification_Allowed_Err.create('No modification allowed error.');
  FContentType:= value;
  case value of
    AS_ANY_CONTENTTYPE,
    AS_EMPTY_CONTENTTYPE,
    AS_UNKNOWN_CONTENTTYPE: FAllowedChildTypes:= [];
    AS_ELEMENT_CONTENTTYPE,
    AS_MIXED_CONTENTTYPE,
    AS_STRICT_MIXED_CONTENTTYPE: FAllowedChildTypes:= [
{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
                                                       AS_ALL_CM,
{$endif}
                                                       AS_CHOICE_CM,
                                                       AS_SEQUENCE_CM];
  end;
end;



{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
//+++++++++++++++++++++++++ TdomASElementDeclNS ++++++++++++++++++++++++++
constructor TdomASElementDeclNS.create(const aOwner: TdomASModelNS;
                                       const aNamespaceURI,
                                             aPrefix,
                                             aLocalName: wideString;
                                       const aContentType: TdomASContentType);
begin
  if aLocalName = ''
    then raise EInvalid_Character_Err.create('Invalid character error.');
  inherited create(aOwner,aNamespaceURI,aPrefix,aLocalName);
  case aContentType of
    AS_ANY_CONTENTTYPE,
    AS_EMPTY_CONTENTTYPE,
    AS_UNKNOWN_CONTENTTYPE: FAllowedChildTypes:= [];
    AS_ELEMENT_CONTENTTYPE,
    AS_MIXED_CONTENTTYPE,
    AS_STRICT_MIXED_CONTENTTYPE: FAllowedChildTypes:= [AS_ALL_CM,
                                                       AS_CHOICE_CM,
                                                       AS_SEQUENCE_CM];
  end;
  FAttributeDeclarations:= TdomASNamedObjectMapNS.create(aOwner);
  FContentType:= aContentType;
  FCreatedContentModels:= TdomASObjectListNS.create;
  FObjectType:= AS_ELEMENT_DECLARATION;
end;

destructor TdomASElementDeclNS.destroy;
begin
  FAttributeDeclarations.free;
  FCreatedContentModels.free;
  inherited;
end;

procedure TdomASElementDeclNS.clear;
var
  i: integer;
begin
  for i:= 0 to pred(FAttributeDeclarations.length) do
    TdomASAttributeDeclNS(FAttributeDeclarations.item(i)).free;
  FAttributeDeclarations.clear;
  for i:= 0 to pred(FCreatedContentModels.length) do
    TdomASContentModelNS(FCreatedContentModels.item(i)).free;
  FCreatedContentModels.clear;
end;

function TdomASElementDeclNS.createContentModel(const contentModelType: TdomASContentModelType): TdomASContentModelNS;
begin
  result:= TdomASContentModelNS.create(self,contentModelType);
  FCreatedContentModels.appendASNode(result);
end;

function TdomASElementDeclNS.findASAttributeDecl(const namespaceURI,
                                                       localName: wideString): TdomASAttributeDeclNS;
begin
  result:= (FAttributeDeclarations.getNamedItem(namespaceURI,localName) as TdomASAttributeDeclNS);
end;

procedure TdomASElementDeclNS.freeAndNilContentModel(var cm: TdomASContentModelNS);
var
  submdl: TdomASContentModelNS;
begin
  if cm.FInuse
    then raise EInuse_Err.create('Content model in use error.');
  // First recursively free the submodels:
  with cm do
    with subModels do
      while length > 0 do begin
        submdl:= removeSubModel(item(pred(length)) as TdomASContentModelNS);
        freeAndNilContentModel(submdl);
      end;
  // Now free the content model:
  FCreatedContentModels.removeASNode(cm);
  cm.free;
  cm:= nil;
end;

function TdomASElementDeclNS.removeASAttributeDecl(const namespaceURI,
                                                         localName: wideString): boolean;
var
  obj: TdomASObjectNS;
begin
  obj:= FAttributeDeclarations.getNamedItem(namespaceURI,localName);
  if assigned(obj) then begin
    FAttributeDeclarations.removeNamedItem(namespaceURI,localName);
    obj.Free;
    result:= true;
  end else result:= false;
end;

function TdomASElementDeclNS.replaceContentModel(const newContentModel: TdomASContentModelNS): TdomASContentModelNS;
begin
  if assigned(newContentModel) then begin
    if newContentModel.ownerModel <> ownerModel
      then raise EAS_Wrong_Element_Decl_Err.create('Wrong element declaration error.');
    if not (newContentModel.contentModelType in FAllowedChildTypes)
      then raise ENot_Supported_Err.create('Not supported error.');
  end;
  FContentModel.FInuse:= false;
  result:= FContentModel;
  newContentModel.FInuse:= true;
  FContentModel:= newContentModel;
end;

function TdomASElementDeclNS.setASAttributeDecl(const namespaceURI,
                                                      prefix,
                                                      localName: wideString;
                                                  out attributeDecl: TdomASAttributeDeclNS): boolean;
begin
  attributeDecl:= findASAttributeDecl(namespaceURI,localName);
  if assigned(attributeDecl) then begin
    result:= false;
  end else begin
    attributeDecl:= TdomASAttributeDeclNS.create(self,namespaceURI,prefix,localName);
    FAttributeDeclarations.setNamedItem(attributeDecl);
    result:= true;
  end;
end;
{$endif}



//+++++++++++++++++++++++++++++ TdomASModel +++++++++++++++++++++++++++++
constructor TdomASModel.create(const aOwner: TdomASModelCollection);
begin
  inherited create(aOwner);
  FElementDeclarations:= TdomASNamedObjectMap.create(self);
  FEntityDeclarations:= TdomASNamedObjectMap.create(self);
  FNotationDeclarations:= TdomASNamedObjectMap.create(self);
end;

destructor TdomASModel.destroy;
begin
  clear;
  FElementDeclarations.free;
  FEntityDeclarations.free;
  FNotationDeclarations.free;
  inherited;
end;

procedure TdomASModel.clear;
var
  i: integer;
begin
  for i:= 0 to pred(FElementDeclarations.length) do
    TdomASElementDecl(FElementDeclarations.item(i)).free;
  FElementDeclarations.clear;
  for i:= 0 to pred(FEntityDeclarations.length) do
    TdomASEntityDecl(FEntityDeclarations.item(i)).free;
  FEntityDeclarations.clear;
  for i:= 0 to pred(FNotationDeclarations.length) do
    TdomASNotationDecl(FNotationDeclarations.item(i)).free;  
  FNotationDeclarations.clear;
end;

function TdomASModel.findASAttributeDecl(const elementName,
                                               attributeName: wideString): TdomASAttributeDecl;
var
  elDecl: TdomASElementDecl;
begin
  elDecl:= findASElementDecl(elementName);
  if assigned(elDecl)
    then result:= elDecl.findASAttributeDecl(attributeName)
    else result:= nil;
end;

function TdomASModel.findASElementDecl(const name: wideString): TdomASElementDecl;
begin
  result:= (FElementDeclarations.getNamedItem(name) as TdomASElementDecl);
end;

function TdomASModel.findASEntityDecl(const name: wideString): TdomASEntityDecl;
begin
  result:= (FEntityDeclarations.getNamedItem(name) as TdomASEntityDecl);
end;

function TdomASModel.findASNotationDecl(const name: wideString): TdomASNotationDecl;
begin
  result:= (FNotationDeclarations.getNamedItem(name) as TdomASNotationDecl);
end;

function TdomASModel.getDomImplementation: TdomImplementation;
begin
  result := OwnerCollection.DomImplementation;
end;

function TdomASModel.getOwnerCollection: TdomASModelCollection;
begin
  result := GetOwner as TdomASModelCollection;
end;

function TdomASModel.removeASElementDecl(const name: wideString): boolean;
var
  obj: TdomASObject;
begin
  obj:= FElementDeclarations.getNamedItem(name);
  if assigned(obj) then begin
    FElementDeclarations.removeNamedItem(name);
    obj.Free;
    result:= true;
  end else result:= false;
end;

function TdomASModel.removeASEntityDecl(const name: wideString): boolean;
var
  obj: TdomASObject;
begin
  obj:= FEntityDeclarations.getNamedItem(name);
  if assigned(obj) then begin
    FEntityDeclarations.removeNamedItem(name);
    obj.Free;
    result:= true;
  end else result:= false;
end;

function TdomASModel.removeASNotationDecl(const name: wideString): boolean;
var
  obj: TdomASObject;
begin
  obj:= FNotationDeclarations.getNamedItem(name);
  if assigned(obj) then begin
    FNotationDeclarations.removeNamedItem(name);
    obj.Free;
    result:= true;
  end else result:= false;
end;

function TdomASModel.setASElementDecl(const name: wideString;
                                      const contentType: TdomASContentType;
                                        out elementDecl: TdomASElementDecl): boolean;
begin
  elementDecl:= findASElementDecl(name);
  if assigned(elementDecl) then begin
    if elementDecl.contentType = AS_UNKNOWN_CONTENTTYPE then begin
      elementDecl.contentType:= contentType;
      result:= true;
    end else result:= false;
  end else begin
    elementDecl:= TdomASElementDecl.create(self,name,contentType);
    FElementDeclarations.setNamedItem(elementDecl);
    result:= true;
  end;
end;

function TdomASModel.setASEntityDecl(const name,
                                           replacementText,
                                           publicId,
                                           systemId,
                                           notationName: wideString;
                                       out entityDecl: TdomASEntityDecl): boolean;
begin
  entityDecl:= findASEntityDecl(name);
  if assigned(entityDecl) then begin
    result:= false;
  end else begin
    entityDecl:= TdomASEntityDecl.create(self,name,replacementText,publicId,systemId,notationName);
    FEntityDeclarations.setNamedItem(entityDecl);
    result:= true;
  end;
end;

function TdomASModel.setASNotationDecl(const name,
                                             publicId,
                                             systemId: wideString;
                                         out notationDecl: TdomASNotationDecl): boolean;
begin
  notationDecl:= findASNotationDecl(name);
  if assigned(notationDecl) then begin
    result:= false;
  end else begin
    notationDecl:= TdomASNotationDecl.create(self,name,publicId,systemId);
    FNotationDeclarations.setNamedItem(notationDecl);
    result:= true;
  end;
end;



{$ifdef INCLUDE_NAMESPACE_ABSTRACT_SCHEMA_MODEL}
//++++++++++++++++++++++++++ TdomCustomASModelNS ++++++++++++++++++++++++++
constructor TdomCustomASModelNS.create(const aOwner: TDomImplementation);
begin
  inherited create;
  FOwnerCollections:= TList.Create;
  FDomImpl:= aOwner;
end;

destructor TdomCustomASModelNS.destroy;
begin
  FOwnerCollections.free;
  inherited;
end;



//++++++++++++++++++++++++++++ TdomASModelNS ++++++++++++++++++++++++++++
constructor TdomASModelNS.create(const aOwner: TDomImplementation);
begin
  inherited create(aOwner);
  FElementDeclarations:= TdomASNamedObjectMapNS.create(self);
  FEntityDeclarations:= TdomASNamedObjectMapNS.create(self);
  FNotationDeclarations:= TdomASNamedObjectMapNS.create(self);
  FIsNamespaceAware:= true;
end;

destructor TdomASModelNS.destroy;
begin
  clear;
  FElementDeclarations.free;
  FEntityDeclarations.free;
  FNotationDeclarations.free;
  inherited;
end;

procedure TdomASModelNS.clear;
var
  i: integer;
begin
  for i:= pred(FOwnerCollections.count) downto 0 do
    TdomASModelCollectionNS(FOwnerCollections[i]).remove(self);
  for i:= 0 to pred(FElementDeclarations.length) do
    TdomASElementDeclNS(FElementDeclarations.item(i)).free;
  FElementDeclarations.clear;
  for i:= 0 to pred(FEntityDeclarations.length) do
    TdomASEntityDeclNS(FEntityDeclarations.item(i)).free;
  FEntityDeclarations.clear;
  for i:= 0 to pred(FNotationDeclarations.length) do
    TdomASNotationDeclNS(FNotationDeclarations.item(i)).free;
  FNotationDeclarations.clear;
end;

function TdomASModelNS.findASAttributeDecl(const elementNamespaceURI,
                                                 elementLocalName,
                                                 attributeNamespaceURI,
                                                 attributeLocalName: wideString): TdomASAttributeDeclNS;
var
  elDecl: TdomASElementDeclNS;
begin
  elDecl:= findASElementDecl(elementNamespaceURI,elementLocalName);
  if assigned(elDecl)
    then result:= elDecl.findASAttributeDecl(attributeNamespaceURI,attributeLocalName)
    else result:= nil;
end;

function TdomASModelNS.findASElementDecl(const aNamespaceURI,
                                               aLocalName: wideString): TdomASElementDeclNS;
begin
  result:= (FElementDeclarations.getNamedItem(aNamespaceURI,aLocalName) as TdomASElementDeclNS);
end;

function TdomASModelNS.findASEntityDecl(const aNamespaceURI,
                                              aLocalName: wideString): TdomASEntityDeclNS;
begin
  result:= (FEntityDeclarations.getNamedItem(aNamespaceURI,aLocalName) as TdomASEntityDeclNS);
end;

function TdomASModelNS.findASNotationDecl(const aNamespaceURI,
                                                aLocalName: wideString): TdomASNotationDeclNS;
begin
  result:= (FNotationDeclarations.getNamedItem(aNamespaceURI,aLocalName) as TdomASNotationDeclNS);
end;

function TdomASModelNS.removeASElementDecl(const aNamespaceURI,
                                                 aLocalName: wideString): boolean;
var
  obj: TdomASObjectNS;
begin
  obj:= FElementDeclarations.getNamedItem(aNamespaceURI,aLocalName);
  if assigned(obj) then begin
    FElementDeclarations.removeNamedItem(aNamespaceURI,aLocalName);
    obj.Free;
    result:= true;
  end else result:= false;
end;

function TdomASModelNS.removeASEntityDecl(const aNamespaceURI,
                                                aLocalName: wideString): boolean;
var
  obj: TdomASObjectNS;
begin
  obj:= FEntityDeclarations.getNamedItem(aNamespaceURI,aLocalName);
  if assigned(obj) then begin
    FEntityDeclarations.removeNamedItem(aNamespaceURI,aLocalName);
    obj.Free;
    result:= true;
  end else result:= false;
end;

function TdomASModelNS.removeASNotationDecl(const aNamespaceURI,
                                                  aLocalName: wideString): boolean;
var
  obj: TdomASObjectNS;
begin
  obj:= FNotationDeclarations.getNamedItem(aNamespaceURI,aLocalName);
  if assigned(obj) then begin
    FNotationDeclarations.removeNamedItem(aNamespaceURI,aLocalName);
    obj.Free;
    result:= true;
  end else result:= false;
end;

function TdomASModelNS.setASElementDecl(const aNamespaceURI,
                                              aPrefix,
                                              aLocalName: wideString;
                                        const contentType: TdomASContentType;
                                          out elementDecl: TdomASElementDeclns): boolean;
begin
  elementDecl:= findASElementDecl(aNamespaceURI,aLocalName);
  if assigned(elementDecl) then begin
    result:= false;
  end else begin
    elementDecl:= TdomASElementDeclNS.create(self,aNamespaceURI,aPrefix,aLocalName,contentType);
    FElementDeclarations.setNamedItem(elementDecl);
    result:= true;
  end;
end;

function TdomASModelNS.setASEntityDecl(const aNamespaceURI,
                                             aPrefix,
                                             aLocalName,
                                             aReplacementText,
                                             aPublicId,
                                             aSystemId,
                                             aNotationName: wideString;
                                         out entityDecl: TdomASEntityDeclNS): boolean;
begin
  entityDecl:= findASEntityDecl(aNamespaceURI,aLocalName);
  if assigned(entityDecl) then begin
    result:= false;
  end else begin
    entityDecl:= TdomASEntityDeclNS.create(self,aNamespaceURI,aPrefix,aLocalName,aReplacementText,aPublicId,aSystemId,aNotationName);
    FEntityDeclarations.setNamedItem(entityDecl);
    result:= true;
  end;
end;

function TdomASModelNS.setASNotationDecl(const aNamespaceURI,
                                               aPrefix,
                                               aLocalName,
                                               publicId,
                                               systemId: wideString;
                                           out notationDecl: TdomASNotationDeclNS): boolean;
begin
  notationDecl:= findASNotationDecl(aNamespaceURI,aLocalName);
  if assigned(notationDecl) then begin
    result:= false;
  end else begin
    notationDecl:= TdomASNotationDeclNS.create(self,aNamespaceURI,aPrefix,aLocalName,publicId,systemId);
    FNotationDeclarations.setNamedItem(notationDecl);
    result:= true;
  end;
end;
{$endif}



// +++++++++++++++++++++++++++ TXmlSourceCode ++++++++++++++++++++++++++
procedure TXmlSourceCode.calculatePieceOffset(const startItem: integer);
var
  os, i: integer;
begin
  if (startItem < count) and (startItem >= 0) then begin
    if startItem = 0
      then os:= 0
      else begin
        if not assigned(Items[startItem-1])
          then begin
            pack;
            exit;
          end else with TXmlSourceCodePiece(Items[startItem-1]) do
            os:= FOffset + length(FText);
      end;
    for i:= startItem to count -1 do
      if not assigned(Items[i])
        then begin
          pack;
          exit;
        end else with TXmlSourceCodePiece(Items[i]) do begin
          FOffset:= os;
          os:= os + length(FText);
        end;
  end; {if ...}
end;

function TXmlSourceCode.getNameOfFirstTag: wideString;
var
  i,j,k: integer;
begin
  result:= '';
  for i:= 0 to count -1 do
    if assigned(Items[i]) then
      with TXmlSourceCodePiece(Items[i]) do
        if (pieceType = xmlStartTag) or (pieceType = xmlEmptyElementTag) then begin
          if pieceType = xmlStartTag
            then k:= length(text)-1
            else k:= length(text)-2;
          j:= 1;
          while j < k do begin
            inc(j);
            if IsXmlWhiteSpace(text[j]) then break;
            Result:= concat(Result,wideString(WideChar(text[j])));
          end;
          exit;
        end;
end;

function TXmlSourceCode.getText: wideString;
var
  content: TUtilsCustomWideStr;
  i: integer;
begin
  content:= TUtilsCustomWideStr.create;
  try
    content.AddWideChar(#$FFEF);  // Add byte order mark.
    for i:= 0 to Pred(Count) do
      content.addWideString(TXmlSourceCodePiece(items[i]).text);
    result:= content.Value;
  finally
    content.free;
  end;
end;

function TXmlSourceCode.Add(Item: pointer): Integer;
begin
  if assigned(Item) then begin
    if not assigned(TXmlSourceCodePiece(Item).FOwner)
      then TXmlSourceCodePiece(Item).FOwner:= self
      else Error('Inuse source code piece error.',-1);
  end else Error('Item not assigned error.',-1);
  Result:= inherited Add(Item);
  calculatePieceOffset(Result);
end;

procedure TXmlSourceCode.Clear;
var
  i: integer;
begin
  for i:= 0 to count -1 do
    if assigned(Items[i]) then
      with TXmlSourceCodePiece(Items[i]) do begin
        FOffset:= 0;
        FOwner:= nil;
      end;
  inherited clear;
end;

procedure TXmlSourceCode.ClearAndFree;
var
  i: integer;
begin
  for i:= 0 to count -1 do
    if assigned(Items[i]) then TXmlSourceCodePiece(Items[i]).free;
  inherited clear;
end;

procedure TXmlSourceCode.Delete(Index: Integer);
begin
  if assigned(Items[index]) then
    with TXmlSourceCodePiece(Items[index]) do begin
      FOffset:= 0;
      FOwner:= nil;
    end;
  inherited Delete(index);
  calculatePieceOffset(Index);
end;

procedure TXmlSourceCode.Exchange(Index1, Index2: Integer);
var
  nr: integer;
begin
  nr:= MinIntValue([Index1,Index2]);
  inherited Exchange(Index1,Index2);
  calculatePieceOffset(nr);
end;

function TXmlSourceCode.GetPieceAtPos(pos: integer): TXmlSourceCodePiece;
var
  i: integer;
begin
  // xxx This search routine is not optimized.
  Result:= nil;
  if pos < 1 then exit;
  for i:= 0 to count -1 do
    if not assigned(Items[i]) then begin
      pack;
      Result:= getPieceAtPos(pos);
    end else with TXmlSourceCodePiece(Items[i]) do begin
      if (FOffset + length(FText)) >= pos then begin
        Result:= TXmlSourceCodePiece(Items[i]);
        exit;
      end;
    end;
end;

procedure TXmlSourceCode.Insert(Index: Integer; Item: pointer);
begin
  if assigned(Item) then begin
    if not assigned(TXmlSourceCodePiece(Item).FOwner)
      then TXmlSourceCodePiece(Item).FOwner:= self
      else Error('Inuse source code piece error.',-1);
  end else Error('Item not assigned error.',-1);
  inherited Insert(Index,item);
  calculatePieceOffset(index);
end;

procedure TXmlSourceCode.Move(CurIndex, NewIndex: Integer);
var
  nr: integer;
begin
  nr:= MinIntValue([CurIndex,NewIndex]);
  inherited Move(CurIndex, NewIndex);
  calculatePieceOffset(nr);
end;

procedure TXmlSourceCode.Pack;
begin
  inherited pack;
  calculatePieceOffset(0);
end;

function TXmlSourceCode.Remove(Item: pointer): Integer;
var
  nr: integer;
begin
  nr:= IndexOf(Item);
  result:= inherited Remove(Item);
  if assigned(Items[nr]) then
    with TXmlSourceCodePiece(Item) do begin
      FOffset:= 0;
      FOwner:= nil;
    end;
  calculatePieceOffset(nr);
end;

procedure TXmlSourceCode.Sort(Compare: TListSortCompare);
begin
  inherited Sort(Compare);
  calculatePieceOffset(0);
end;



// ++++++++++++++++++++++++ TXmlSourceCodePiece ++++++++++++++++++++++++
constructor TXmlSourceCodePiece.create(const pt: TdomPieceType);
begin
  FPieceType:= pt;
  Ftext:= '';
  FOffset:= 0;
  FOwner:= nil;
end;



// +++++++++++++++++++++++ TStandardResourceResolver +++++++++++++++++++++++
function TStandardResourceResolver.acquireStreamFromUri(const URI: wideString): TStream;
var
  Path: TFilename;
  Authority, Query, Fragment: string; // Only dummies.
  UriAnalyzer: TUriStrAnalyzer;
begin
  UriAnalyzer := TUriStrAnalyzer.create;
  try
    with UriAnalyzer do begin
      SetUriReference(URI);
      if not HasUriScheme then
        raise EFOpenError.CreateFmt('URI "%s" contains no scheme.', [URI]);
      if UriScheme <> 'file' then
        raise EFOpenError.CreateFmt('URI scheme "%s" not supported.', [UriScheme]);

      UriStrToFilename(URI, Path, Authority, Query, Fragment);
      if not FileExists(Path) then
        raise EFOpenError.CreateFmt('File "%s" not found.', [ExpandFileName(Path)]);
      Result := TFileStream.Create(Path, fmOpenRead);

    end;
  finally
    UriAnalyzer.Free;
  end;
end;

function TStandardResourceResolver.ResolveResource(const aBaseURI: wideString;
                                                     var publicId,
                                                         systemId: wideString): TStream;
// Remark: resourceType and namespaceURI are currently not evaluated.  They are
//         placeholders for XML Schema support in the future.  certifiedText is
//         also not evaluated.  It is a placeholder for XML 1.1 support.
var
  resourceType, namespaceURI: wideString;
  certifiedText: boolean;
  Uri: wideString;
begin
  resourceType:= 'http://www.w3.org/TR/REC-xml';  // Signals an XML 1.0 resource.
  namespaceURI:= '';                              // Currently not used.
  certifiedText:= false;                          // Currently not used.

  result := nil;

  // Calculate absolute system identifier:
  if ResolveRelativeUriWideStr(ABaseUri, systemId, Uri)
    then systemId := Uri
    else systemId := '';

  if assigned(FOnResolveResource) then
    FOnResolveResource(self, resourceType, namespaceURI, publicId, systemId, result, certifiedText);

  if not Assigned(result) and (SystemId <> '') then begin
    try
      result := AcquireStreamFromURI(SystemId);
    except
      result.Free;
      result := nil;
    end;
  end;
end;



{ TXmlCustomInputSource }

constructor TXmlInputSource.create(const stream: TStream;
                                   const aPublicId,
                                         aSystemId: wideString;
                                   const aBufSize: integer;
                                   const defaultEncoding: wideString;
                                   const inclDecl: boolean;
                                   const initialByteCount,
                                         initialCharCount,
                                         initialRegularCharsInLine,
                                         initialTabsInLine,
                                         initialLine: Int64);
var
  DefaultCodecClass: TUnicodeCodecClass;
begin
  FPublicId := APublicId;
  FSystemId := ASystemId;

  // Find out whether XDOM supports the specified encoding.  If not set the
  // default encoding to UTF-8:
  DefaultCodecClass := StrToEncoding(DefaultEncoding);
  if Assigned(DefaultCodecClass) then begin
    FInputEncoding := DefaultEncoding;
  end else begin
    FInputEncoding := 'UTF-8';
    DefaultCodecClass := TUTF8Codec;
  end;

  inherited Create(Stream, ABufSize, DefaultCodecClass, InitialByteCount,
      InitialCharCount, InitialRegularCharsInLine, InitialTabsInLine,
      InitialLine); // xxx Look first for XML declaration in UTF-8?

  if CodecClass <> DefaultCodecClass then
    FInputEncoding := GetEncodingName(CodecClass);

  FHasMalformedDecl := not EvaluateXmlOrTextDecl(FDeclType, FXmlVersion,
                             FXmlEncoding, FXmlStandalone, FInvalidEncoding);

  if not inclDecl then
    InitialUCS4CharData := CurrentCharInfo;

  Reset;
end;

function TXmlInputSource.evaluateXmlOrTextDecl(out declType: TdomXMLDeclType;
                                               out version,
                                                   encName: wideString;
                                               out standalone: TdomStandalone;
                                               out invalidEnc: boolean): boolean;
var
  NewCodecClass: TUnicodeCodecClass;
  QM: UCS4Char;
  WhitespaceSkipped: Boolean;
begin
  Result := True;
  DeclType := DT_UNKNOWN;
  EncName := '';
  Version := '';
  Standalone := STANDALONE_UNSPECIFIED;
  InvalidEnc := False;
  try
    if Match('<?xml') then begin // Does the stream start with '<?xml'?
      DeclType := DT_XML_OR_TEXT_DECLARATION;

      WhitespaceSkipped := SkipNext(GetXmlWhitespaceWideString) > 0;

      // version:
      if CurrentCharInfo.CodePoint = $0076 then begin // 'v'
        if not WhitespaceSkipped then begin
          Result := False;
          Exit;
        end;
        if Match('ersion') then begin
          SkipNext(GetXmlWhitespaceWideString);
          if not ( CurrentCharInfo.CodePoint = $003D ) then begin  // '='
            Result := False;
            Exit;
          end;
          SkipNext(GetXmlWhitespaceWideString);
          if not ( ( CurrentCharInfo.CodePoint = $0022 ) or
                   ( CurrentCharInfo.CodePoint = $0027 ) ) then begin  // '"' or '''
            Result := False;
            Exit;
          end;
          QM := CurrentCharInfo.CodePoint;
          Next;
          if IsXmlVersionNumCharCodePoint(CurrentCharInfo.CodePoint) then begin
            version:= wideString(wideChar(CurrentCharInfo.CodePoint));
          end else begin
            Result := False;
            Exit;
          end;
          Next;
          while IsXmlVersionNumCharCodePoint(CurrentCharInfo.CodePoint) do begin
            version:= concat(version, wideString(wideChar(CurrentCharInfo.CodePoint)));
            Next;
          end;
          if CurrentCharInfo.CodePoint <> QM then begin  // Is the first quotation mark of the same type as the second?
            Result := False;
            Exit;
          end;
          WhitespaceSkipped := SkipNext(GetXmlWhitespaceWideString) > 0;
        end else begin
          Result := False;
          Exit;
        end; {if ... else ...}
      end else DeclType := DT_TEXT_DECLARATION;

      // EncodingDecl:
      if CurrentCharInfo.CodePoint = $0065 then begin // 'e'
        if not WhitespaceSkipped then begin
          result := False;
          Exit;
        end;
        if Match('ncoding') then begin
          SkipNext(GetXmlWhitespaceWideString);
          if not ( CurrentCharInfo.CodePoint = $003D ) then begin  // '='
            Result := False;
            Exit;
          end;
          SkipNext(GetXmlWhitespaceWideString);
          if not ( ( CurrentCharInfo.CodePoint = $0022 ) or
                   ( CurrentCharInfo.CodePoint = $0027 ) ) then begin  // '"' or '''
            Result := False;
            Exit;
          end;
          QM := CurrentCharInfo.CodePoint;
          Next;
          if IsXmlEncNameLeadingCharCodePoint(CurrentCharInfo.CodePoint) then begin
            EncName:= wideString(wideChar(CurrentCharInfo.CodePoint));
          end else begin
            Result := False;
            Exit;
          end;
          Next;
          while IsXmlEncNameFollowingCharCodePoint(CurrentCharInfo.CodePoint) do begin
            EncName:= concat(EncName, wideString(wideChar(CurrentCharInfo.CodePoint)));
            Next;
          end;
          if CurrentCharInfo.CodePoint <> QM then begin  // Is the first quotation mark of the same type as the second?
            Result := False;
            Exit;
          end;
          WhitespaceSkipped := SkipNext(GetXmlWhitespaceWideString) > 0;
        end else begin
          Result := False;
          Exit;
        end; {if ... else ...}
      end else begin
        if declType = DT_TEXT_DECLARATION then begin
          Result := False;
          Exit;
        end else DeclType := DT_XML_DECLARATION;
      end; {if ... else ...}

      // SDDecl:
      if CurrentCharInfo.CodePoint = $0073 then begin // 's'
        if not WhitespaceSkipped then begin
          Result := False;
          Exit;
        end;
        if Match('tandalone') then begin
          SkipNext(GetXmlWhitespaceWideString);
          if not ( CurrentCharInfo.CodePoint = $003D ) then begin  // '='
            Result := False;
            Exit;
          end;
          SkipNext(GetXmlWhitespaceWideString);
          if not ( ( CurrentCharInfo.CodePoint = $0022 ) or
                   ( CurrentCharInfo.CodePoint = $0027 ) ) then begin  // '"' or '''
            Result := False;
            Exit;
          end;
          QM := CurrentCharInfo.CodePoint;
          Next;

          case CurrentCharInfo.CodePoint of
            $0079: begin // 'y'
              Next;
              if CurrentCharInfo.CodePoint = $0065 then begin  // 'e'
                Next;
                if CurrentCharInfo.CodePoint = $0073 then begin // 's'
                  Standalone:= STANDALONE_YES;
                end else begin
                  Result := False;
                  Exit;
                end;
              end else begin
                Result := False;
                Exit;
              end;
            end;
            $006e: begin // 'n'
              Next;
              if CurrentCharInfo.CodePoint = $006f then begin // 'o'
                Standalone:= STANDALONE_NO;
              end else begin
                Result := False;
                Exit;
              end;
            end;
          else
            Result := False;
            Exit;
          end; {case ...}
          Next;
          if CurrentCharInfo.CodePoint <> QM then begin  // Is the first quotation mark of the same type as the second?
            Result := False;
            Exit;
          end;
          SkipNext(GetXmlWhitespaceWideString);
        end else begin
          Result := False;
          Exit;
        end; {if ... else ...}
        if declType = DT_TEXT_DECLARATION then begin
          Result := False;
          Exit;
        end else DeclType := DT_XML_DECLARATION;
      end; {if ...}

      // '?>':
      if (CurrentCharInfo.CodePoint = $003F) // '?'
        and Match('>') then begin   // '>'

        // Calculate FEncoding:
        if EncName <> '' then begin

          try
            NewCodecClass := StrToEncoding(UTF16ToEncoding(TUSASCIICodec, EncName));
          except
            NewCodecClass := nil;
          end;

          if assigned(NewCodecClass)then begin

            if ( (ByteOrderMarkType = TUTF16BECodec) and (NewCodecClass <> TUTF16BECodec) and (NewCodecClass <> TUCS2Codec) ) or
               ( (ByteOrderMarkType = TUTF16LECodec) and (NewCodecClass <> TUTF16LECodec) ) or
               ( (ByteOrderMarkType = TUTF8Codec)    and (NewCodecClass <> TUTF8Codec)    )
              then InvalidEnc:= True; // Byte order mark does not fit to encoding declaration.
            SetCodecClass(NewCodecClass);

          end else begin
            if CompareText(UTF16ToEncoding(TUSASCIICodec, EncName), 'UTF-16') = 0 then begin
              if not Assigned(ByteOrderMarkType) then begin
                SetCodecClass(TUTF16BECodec);
                // Cf. RFC 2781: "UTF-16, an encoding of ISO 10646", sec. 4.3:
                //   If the first two octets of the text is not 0xFE followed by
                //   0xFF, and is not 0xFF followed by 0xFE, then the text SHOULD be
                //   interpreted as being big-endian.
              end else if not ( (ByteOrderMarkType = TUTF16BECodec) or
                                (ByteOrderMarkType = TUTF16LECodec) ) then
                InvalidEnc := True;
            end else
              InvalidEnc := True;
          end;

        end; {if EncName ...}

        ResetPosition := BOMReader.Position - NextCharInfo.Size;

      end else
        Result := False;

    end else begin
      DeclType := DT_UNSPECIFIED;
      Reset;
    end;

  except
    Result := False;
  end; {try ...}
end;

function TXmlInputSource.GetCurrentCodePoint: UCS4Char;
begin
  Result := CurrentCharInfo.CodePoint;
end;

function TXmlInputSource.GetNextCodePoint: UCS4Char;
begin
  Result := NextCharInfo.CodePoint;
end;

function TXmlInputSource.GetPreviousCodePoint: UCS4Char;
begin
  Result := PreviousCharInfo.CodePoint;
end;



{ TXmlCustomTokenizer }

constructor TXmlCustomTokenizer.create(const aInputSource: TXmlInputSource;
                                       const aTabWidth: cardinal);
begin
  inherited Create;
  FTabWidth := ATabWidth;
  FTokenValue := TUtilsCustomWideStr.Create;
  FInputSource := AInputSource;
  FTokenStart := AInputSource.CurrentCharInfo;
  FTokenEnd := AInputSource.PreviousCharInfo;
  FErrorType := ET_NONE;
end;

destructor TXmlCustomTokenizer.destroy;
begin
  FTokenValue.Free;
  inherited;
end;

function TXmlCustomTokenizer.GetEndByteNumber: Int64;
begin
  Result := FTokenEnd.ByteCount;
end;

function TXmlCustomTokenizer.GetEndCharNumber: Int64;
begin
  Result := FTokenEnd.CharCount;
end;

function TXmlCustomTokenizer.GetEndColumnNumber: Int64;
begin
  with FTokenEnd do
    Result := RegularCharsInLine + (TabWidth * TabsInLine);
end;

function TXmlCustomTokenizer.GetEndLineNumber: Int64;
begin
  Result := FTokenEnd.Line;
end;

function TXmlCustomTokenizer.GetRelatedASObject: TdomASObject;
begin
  Result := nil;
end;

function TXmlCustomTokenizer.GetRelatedNode: TdomNode;
begin
  Result := nil;
end;

function TXmlCustomTokenizer.GetStartByteNumber: Int64;
begin
  with FTokenStart do
    Result := ByteCount - Size;
end;

function TXmlCustomTokenizer.GetStartCharNumber: Int64;
begin
  Result := FTokenStart.CharCount;
end;

function TXmlCustomTokenizer.GetStartColumnNumber: Int64;
begin
  with FTokenStart do
    Result := RegularCharsInLine + (TabWidth * TabsInLine);
end;

function TXmlCustomTokenizer.GetStartLineNumber: Int64;
begin
  Result := FTokenStart.Line;
end;

function TXmlCustomTokenizer.getTokenValue: wideString;
begin
  Result := FTokenValue.Value;
end;

function TXmlCustomTokenizer.GetUri: WideString;
begin
  if Assigned(FInputSource)
    then Result := FInputSource.SystemId
    else Result := '';
end;



{ TXmlDocTokenizer }

constructor TXmlDocTokenizer.create(const aInputSource: TXmlInputSource;
                                    const aTabWidth: cardinal);
begin
  inherited;
  if Assigned(AInputSource)
    then FTokenType:= XML_START_OF_SOURCE_TOKEN
    else FTokenType:= XML_END_OF_SOURCE_TOKEN;
end;

procedure TXmlDocTokenizer.next;
const
  EM_CODE          = $21; // code of !
  DQ_CODE          = $22; // code of "
  NUMBER_CODE      = $23; // code of #
  AMP_CODE         = $26; // code of &
  SQ_CODE          = $27; // code of '
  HYPHEN_CODE      = $2D; // code of -
  SOLIDUS_CODE     = $2F; // code of /
  COLON_CODE       = $3A; // code of :
  SEMICOLON_CODE   = $3B; // code of ;
  LT_CODE          = $3C; // code of <
  GT_CODE          = $3E; // code of >
  QM_CODE          = $3F; // code of ?
  CAPITAL_C_CODE   = $43; // code of C
  CAPITAL_D_CODE   = $44; // code of D
  CAPITAL_O_CODE   = $4F; // code of O
  LS_BRACKET_CODE  = $5B; // code of [
  RS_BRACKET_CODE  = $5D; // code of ]
  LOW_LINE_CODE    = $5F; // code of _
  SMALL_C_CODE     = $63; // code of c
  SMALL_D_CODE     = $64; // code of d
  SMALL_O_CODE     = $6F; // code of o
  SMALL_X_CODE     = $78; // code of x
  STRING_TERMINATOR_CODE = $9C;

  CDATA_START: array[0..5] of UCS4Char =
    (ord('C'), ord('D'), ord('A'), ord('T'), ord('A'), ord('['));
  DOCTYPE_START: array[0..5] of UCS4Char =
    (ord('O'), ord('C'), ord('T'), ord('Y'), ord('P'), ord('E'));

  PIEND: wideString = '?>';
var
  i: integer;
  subEndMarker,SubStartMarker: wideString;
  SQ_Open,DQ_Open,Bracket_Open: boolean;
begin
  if FTokenType = XML_END_OF_SOURCE_TOKEN then exit;

  FTokenValue.Clear;
  FErrorType:= ET_NONE;
  FClue:= '';
  FTokenStart := FInputSource.NextCharInfo;

  try
    FInputSource.Next;

    case FInputSource.CurrentCodePoint of

      // '<' found:
      LT_CODE: begin
        case FInputSource.NextCodePoint of

          // '/' --> End Tag found:
          SOLIDUS_CODE: begin
            FTokenType:= XML_END_TAG_TOKEN;
            FInputSource.Next;
            FTokenStart := FInputSource.NextCharInfo;
            while not ( IsXmlWhiteSpaceCodePoint(FInputSource.NextCodePoint) or
                        (FInputSource.NextCodePoint = GT_CODE) or // '>'
                        (FInputSource.NextCodePoint = STRING_TERMINATOR_CODE) ) do begin
              FInputSource.Next;
              FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
            end;
            FTokenEnd := FInputSource.CurrentCharInfo;
            while IsXmlWhiteSpaceCodePoint(FInputSource.NextCodePoint) do // Skip whitespace.
              FInputSource.Next;
            if FInputSource.NextCodePoint = GT_CODE then begin// '>' ?
              FInputSource.Next;
            end else begin
              FErrorType:= ET_UNCLOSED_ELEMENT;
              FClue:= '>';
            end;
          end;

          // '?' --> Processing Instruction found:
          QM_CODE: begin
            FTokenType:= XML_PI_TOKEN;
            FInputSource.Next;
            FTokenStart := FInputSource.NextCharInfo;
            while FInputSource.NextCodePoint <> STRING_TERMINATOR_CODE do begin
              FInputSource.Next;
              if (FInputSource.CurrentCodePoint = QM_CODE) and
                 (FInputSource.NextCodePoint = GT_CODE)
              then begin
                // '?>' found:
                FTokenEnd := FInputSource.PreviousCharInfo;
                FInputSource.Next;
                Exit;
              end;
              FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
            end;
            FTokenEnd := FInputSource.PreviousCharInfo;
            FErrorType:= ET_UNCLOSED_PROCESSING_INSTRUCTION;
            FClue:= '?';
          end;

          // '!' --> Comment, CDATA Section or Document Type Declaration found:
          EM_CODE: begin
            FInputSource.Next;
            case FInputSource.NextCodePoint of

              HYPHEN_CODE: begin // '-' --> Comment found:
                FTokenType:= XML_COMMENT_TOKEN;
                FInputSource.Next;
                if FInputSource.NextCodePoint = HYPHEN_CODE then begin // '<!--' found:
                  FInputSource.Next;
                  FTokenStart := FInputSource.NextCharInfo;
                  while FInputSource.NextCodePoint <> STRING_TERMINATOR_CODE do begin
                    FInputSource.Next;
                    if FInputSource.CurrentCodePoint = HYPHEN_CODE then begin // '-' found
                      FTokenEnd := FInputSource.PreviousCharInfo;
                      FInputSource.Next;
                      case FInputSource.CurrentCodePoint of
                        HYPHEN_CODE: begin // Second '-'
                          if FInputSource.NextCodePoint = GT_CODE then begin// '>'?
                            FInputSource.Next;
                          end else begin
                            FTokenEnd := FInputSource.CurrentCharInfo;
                            FErrorType:= ET_DOUBLE_HYPHEN_IN_COMMENT;
                            FClue:= '>';
                          end;
                          exit;
                        end;
                        STRING_TERMINATOR_CODE: begin
                          FTokenEnd := FInputSource.PreviousCharInfo;
                          FErrorType:= ET_UNCLOSED_COMMENT;
                          FClue:= '-->';
                          exit;
                        end;
                      else
                        // No second '-' --> Add '-' to content of comment:
                        FTokenValue.addUCS4Char(HYPHEN_CODE);
                      end;
                    end;
                    FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
                  end;
                  FTokenEnd := FInputSource.CurrentCharInfo;
                  FErrorType:= ET_UNCLOSED_COMMENT;
                  FClue:= '-->';
                  exit;
                end;
                FTokenEnd := FInputSource.CurrentCharInfo;
                FErrorType:= ET_COMMENT_START_EXPECTED;
                FClue:= '<!--';
              end;

              LS_BRACKET_CODE: begin // '[' --> CDATA Section found:
                FTokenType:= XML_CDATA_TOKEN;
                FInputSource.Next;
                for i:= 0 to 5 do
                  if FInputSource.NextCodePoint = CDATA_START[i] then begin
                    FInputSource.Next;
                  end else begin
                    FTokenEnd := FInputSource.CurrentCharInfo;
                    FErrorType:= ET_CDATA_START_EXPECTED;
                    FClue:= '<![CDATA[';
                    Exit;
                  end;
                FTokenStart := FInputSource.NextCharInfo;
                while FInputSource.NextCodePoint <> STRING_TERMINATOR_CODE do begin
                  FInputSource.Next;
                  while (FInputSource.CurrentCodePoint = RS_BRACKET_CODE) and
                        (FInputSource.NextCodePoint = RS_BRACKET_CODE) do begin
                    // ']]' found:
                    FTokenEnd := FInputSource.PreviousCharInfo;
                    FInputSource.Next;
                    if FInputSource.NextCodePoint = GT_CODE then begin
                      // '>' found:
                      FInputSource.Next;
                      exit;
                    end else FTokenValue.addUCS4Char(RS_BRACKET_CODE);
                  end;
                  FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
                end;
                FTokenEnd := FInputSource.CurrentCharInfo;
                FErrorType:= ET_UNCLOSED_CDATA_SECTION;
                FClue:= ']]>';
              end;

              CAPITAL_D_CODE: begin // 'D' --> Document Type Declaration found:
                FTokenType:= XML_DOCTYPE_TOKEN;
                FInputSource.Next;
                for i:= 0 to 5 do
                  if FInputSource.NextCodePoint = DOCTYPE_START[i] then begin
                    FInputSource.Next;
                  end else begin
                    FErrorType:= ET_DOCTYPE_START_EXPECTED;
                    FClue:= '<!DOCTYPE';
                    exit;
                  end;
                DQ_Open := False;
                SQ_Open := False;
                Bracket_Open := False;
                subStartMarker:= '';
                subEndMarker:= '';
                FTokenStart := FInputSource.NextCharInfo;
                while FInputSource.NextCodePoint <> STRING_TERMINATOR_CODE do begin
                  FInputSource.Next;
                  if (FInputSource.CurrentCodePoint = GT_CODE) // '>'
                     and (not DQ_Open)
                     and (not SQ_Open)
                     and (not Bracket_Open)
                     and (SubEndMarker = '')
                    then begin
                      FTokenEnd := FInputSource.PreviousCharInfo;
                      Exit;
                    end;
                  FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);

                  if (SubEndMarker = '') then begin

                    if (FInputSource.CurrentCodePoint = SQ_CODE) and (not DQ_Open) then begin
                      SQ_Open := not SQ_Open;
                    end else if (FInputSource.CurrentCodePoint = DQ_CODE) and (not SQ_Open) then begin
                      DQ_Open := not DQ_Open;
                    end;

                    if Bracket_Open then begin
                      if not (SQ_Open or DQ_Open) then begin
                        if FInputSource.CurrentCodePoint = LT_CODE then begin  // '<'
                          SubStartMarker:= '<';
                        end else if (FInputSource.CurrentCodePoint = EM_CODE) and (SubStartMarker = '<') then begin // '!'
                          SubStartMarker:= '<!';
                        end else if (FInputSource.CurrentCodePoint = QM_CODE) and (SubStartMarker = '<') then begin // '?'
                          SubStartMarker:= '';
                          SubEndMarker:= PIEND;
                        end else if (FInputSource.CurrentCodePoint = HYPHEN_CODE) and (SubStartMarker = '<!')then begin // '-'
                          SubStartMarker:= '<!-';
                        end else if (FInputSource.CurrentCodePoint = HYPHEN_CODE) and (SubStartMarker = '<!-')then begin // '-'
                          SubStartMarker:= '';
                          SubEndMarker:= '-->';
                        end else if SubStartMarker <> '' then begin
                          SubStartMarker:= '';
                        end;
                        if (FInputSource.CurrentCodePoint = RS_BRACKET_CODE) // ']'
                          and (not SQ_Open)
                          and (not DQ_Open)
                          then Bracket_Open:= false;
                      end; {if not ...}
                    end else begin {if BracketOpened ... }
                      if (FInputSource.CurrentCodePoint = LS_BRACKET_CODE) // '['
                        and (not SQ_Open)
                        and (not DQ_Open) then Bracket_Open:= true;
                    end; {if BracketOpened ... else ...}

                  end else begin; {if (SubEndMarker = '') ...}
                    if FTokenValue.endsWith(SubEndMarker) then SubEndMarker:= '';
                  end; {if (SubEndMarker = '') ... else ...}

                end;
                FTokenEnd := FInputSource.CurrentCharInfo;
                FErrorType:= ET_UNCLOSED_DOCTYPE;
                FClue:= ']>';
              end;

              SMALL_D_CODE, CAPITAL_O_CODE, SMALL_O_CODE: begin // 'd', 'O' 'o' --> Possible Document Type Declaration typo found:
                FTokenEnd := FInputSource.CurrentCharInfo;
                FTokenType:= XML_DOCTYPE_TOKEN;
                FErrorType:= ET_DOCTYPE_START_EXPECTED;
                FClue:= '<!DOCTYPE';
              end;

              RS_BRACKET_CODE, CAPITAL_C_CODE, SMALL_C_CODE: begin // ']' 'C', 'c' --> Possible CDATA section typo found:
                FTokenEnd := FInputSource.CurrentCharInfo;
                FTokenType:= XML_CDATA_TOKEN;
                FErrorType:= ET_CDATA_START_EXPECTED;
                FClue:= '<![CDATA[';
              end;

            else
              FTokenEnd := FInputSource.CurrentCharInfo;
              FTokenType:= XML_COMMENT_TOKEN;
              FErrorType:= ET_COMMENT_START_EXPECTED;
              FClue:= '<!--';
            end;
          end;

          GT_CODE: begin
            FTokenEnd := FInputSource.CurrentCharInfo;
            FInputSource.Next;
            FTokenType:= XML_START_TAG_TOKEN;
            FErrorType:= ET_MISSING_ELEMENT_NAME;
          end;

          STRING_TERMINATOR_CODE: begin
            FTokenEnd := FInputSource.CurrentCharInfo;
            FTokenType:= XML_START_TAG_TOKEN;
            FErrorType:= ET_MISSING_ELEMENT_NAME;
          end;

        else

          // Start Tag or Empty Element Tag found:
          SQ_Open:= False;
          DQ_Open:= False;
          FInputSource.Next;
          FTokenStart := FInputSource.CurrentCharInfo;
          FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
          while not (FInputSource.NextCodePoint in [SOLIDUS_CODE, GT_CODE, STRING_TERMINATOR_CODE]) do begin
            FInputSource.Next;
            FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
            if FInputSource.CurrentCodePoint = SQ_CODE then
              SQ_Open:= True;
            if FInputSource.CurrentCodePoint = DQ_CODE then
              DQ_Open:= True;
            while SQ_Open and (FInputSource.NextCodePoint <> STRING_TERMINATOR_CODE) do begin
              FInputSource.Next;
              FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
              if FInputSource.CurrentCodePoint = SQ_CODE then
                SQ_Open:= False;
            end;
            while DQ_Open and (FInputSource.NextCodePoint <> STRING_TERMINATOR_CODE) do begin
              FInputSource.Next;
              FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
              if FInputSource.CurrentCodePoint = DQ_CODE then
                DQ_Open:= False;
            end;
          end;
          FTokenEnd := FInputSource.CurrentCharInfo;
          case FInputSource.NextCodePoint of
            SOLIDUS_CODE: begin
              FTokenType:= XML_EMPTY_ELEMENT_TAG_TOKEN;
              FInputSource.Next;
              if FInputSource.NextCodePoint = GT_CODE then begin
                FInputSource.Next;
              end else begin
                FErrorType:= ET_UNCLOSED_ELEMENT;
                FClue:= '>';
              end;
            end;
            GT_CODE: begin
              FTokenType:= XML_START_TAG_TOKEN;
              FInputSource.Next;
            end;
            STRING_TERMINATOR_CODE: begin
              FTokenType:= XML_START_TAG_TOKEN;
              FErrorType:= ET_UNCLOSED_ELEMENT;
              if SQ_Open then begin
                FClue:= '''>';
              end else if DQ_Open then begin
                FClue:= '">';
              end else
                FClue:= '>';
            end;
          end;
        end;
      end;

      // Start of reference ('&') found:
      AMP_CODE: begin
        if FInputSource.NextCodePoint = NUMBER_CODE then begin // '#' found --> Character reference.
          FInputSource.Next;

          if FInputSource.NextCodePoint = SMALL_X_CODE then begin // 'x' found --> Hexadecimal character reference.
            FTokenType:= XML_CHAR_REF_HEX_TOKEN;
            FInputSource.Next;
            FTokenStart := FInputSource.NextCharInfo;
            while IsXmlHexDigitCodePoint(FInputSource.NextCodePoint) do begin
              FInputSource.Next;
              FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
            end;
          end else begin // Decimal character reference
            FTokenType:= XML_CHAR_REF_DEC_TOKEN;
            FTokenStart := FInputSource.NextCharInfo;
            while IsXmlDecDigitCodePoint(FInputSource.NextCodePoint) do begin
              FInputSource.Next;
              FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
            end;
          end;
          FTokenEnd := FInputSource.CurrentCharInfo;
          if FInputSource.NextCodePoint = SEMICOLON_CODE then begin // ';' found
            FInputSource.Next;
          end else begin
            FErrorType:= ET_UNCLOSED_CHARREF;
            FClue:= ';';
          end;

        end else begin // Entity reference
          FTokenType:= XML_ENTITY_REF_TOKEN;

          FTokenStart := FInputSource.NextCharInfo;
          if IsXmlLetterCodePoint(FInputSource.NextCodePoint) or
             (FInputSource.NextCodePoint = COLON_CODE) or
             (FInputSource.NextCodePoint = LOW_LINE_CODE)
          then begin
            FInputSource.Next;
            FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
            while IsXmlNameCharCodePoint(FInputSource.NextCodePoint) do begin
              FInputSource.Next;
              FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
            end;
            FTokenEnd := FInputSource.CurrentCharInfo;
            if FInputSource.NextCodePoint = SEMICOLON_CODE then begin// ';' found
              FInputSource.Next;
            end else begin
              FErrorType:= ET_UNCLOSED_ENTITY_REF;
              FClue:= ';';
            end;
          end else begin
            FTokenEnd := FInputSource.CurrentCharInfo;
            FErrorType:= ET_MISSING_ENTITY_NAME;
          end;
        end;
      end;

      // End of source found:
      STRING_TERMINATOR_CODE: begin
        FTokenEnd := FInputSource.CurrentCharInfo;
        FTokenType:= XML_END_OF_SOURCE_TOKEN;
      end;

    else
      // PCDATA found:
      FTokenType:= XML_PCDATA_TOKEN;
      FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
      while not (FInputSource.NextCodePoint in [AMP_CODE, LT_CODE, STRING_TERMINATOR_CODE]) do begin
        FInputSource.Next;
        FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
      end;
      FTokenEnd := FInputSource.CurrentCharInfo;
    end;

  except
    on EConvertError do begin
      FTokenEnd := FInputSource.CurrentCharInfo;
      FErrorType:= ET_INVALID_CHARACTER;
    end;
  end; {try ...}
end;



{ TXmlDoctypeDeclTokenizer }

constructor TXmlDoctypeDeclTokenizer.create(const aInputSource: TXmlInputSource;
                                            const aTabWidth: cardinal);
begin
  inherited;
  if assigned(aInputSource) then begin
    if aInputSource.hasMalformedDecl
       or not ( aInputSource.declType in [ DT_UNSPECIFIED ] )
      then begin
        FTokenType:= DOCTYPE_END_OF_SOURCE_TOKEN;
        FErrorType:= ET_INVALID_MARKUP_DECL;
      end else begin
        FTokenType:= DOCTYPE_START_OF_SOURCE_TOKEN;
        FErrorType:= ET_NONE;
      end;
  end else FTokenType:= DOCTYPE_END_OF_SOURCE_TOKEN;
end;

procedure TXmlDoctypeDeclTokenizer.next;
const
  EM_CODE          = $21; // code of !
  DQ_CODE          = $22; // code of "
  SQ_CODE          = $27; // code of '
  HYPHEN_CODE      = $2D; // code of -
  LT_CODE          = $3C; // code of <
  GT_CODE          = $3E; // code of >
  QM_CODE          = $3F; // code of ?
  CAPITAL_P_CODE   = $50; // code of P
  CAPITAL_S_CODE   = $53; // code of S
  LS_BRACKET_CODE  = $5B; // code of [
  RS_BRACKET_CODE  = $5D; // code of ]
  STRING_TERMINATOR_CODE = $9C;

  PUBLIC_ID_START: array[0..4] of UCS4Char =
    (ord('U'), ord('B'), ord('L'), ord('I'), ord('C'));
  SYSTEM_ID_START: array[0..4] of UCS4Char =
    (ord('Y'), ord('S'), ord('T'), ord('E'), ord('M'));
var
  CommentStartFound: Boolean;
  CommentEndFound: Boolean;
  DoctypeNameStart: Boolean;
  DQ_Open: Boolean;
  EM_Found: Boolean;
  I: Integer;
  InComment: Boolean;
  InPI: Boolean;
  LT_Found: Boolean;
  QuoteCode: UCS4Char;
  SQ_Open: Boolean;
begin
  if FTokenType = DOCTYPE_END_OF_SOURCE_TOKEN then Exit;

  FTokenValue.Clear;
  FErrorType:= ET_NONE;
  FClue:= '';
  FTokenStart := FInputSource.NextCharInfo;

  try

    case FTokenType of
      DOCTYPE_INTSUBSET_TOKEN:
        begin
          // Skip whitespace:
          while isXmlWhiteSpaceCodePoint(FInputSource.NextCodePoint) do
            FInputSource.Next;

          FInputSource.Next;
          if FInputSource.CurrentCodePoint = STRING_TERMINATOR_CODE then begin
            FTokenEnd := FInputSource.PreviousCharInfo;
            FTokenType := DOCTYPE_END_OF_SOURCE_TOKEN
          end else begin
            FTokenEnd := FInputSource.CurrentCharInfo;
            FErrorType := ET_UNCLOSED_DOCTYPE;
          end;
        end;

      DOCTYPE_NAME_TOKEN:
        begin
          // Skip optional whitespace:
          while isXmlWhiteSpaceCodePoint(FInputSource.NextCodePoint) do
            FInputSource.Next;

          case FInputSource.NextCodePoint of
            STRING_TERMINATOR_CODE:
              begin
                FInputSource.Next;
                FTokenEnd := FInputSource.PreviousCharInfo;
                FTokenType := DOCTYPE_END_OF_SOURCE_TOKEN;
              end;
            CAPITAL_P_CODE: // 'P' --> 'PUBLIC' found.
              begin
                FInputSource.Next;
                FTokenStart := FInputSource.CurrentCharInfo;
                for I := 0 to 4 do
                  if FInputSource.NextCodePoint = PUBLIC_ID_START[I] then begin
                    FInputSource.Next;
                  end else begin
                    FTokenEnd := FInputSource.CurrentCharInfo;
                    FErrorType := ET_PUBLIC_KEYWORD_EXPECTED;
                    FClue := 'PUBLIC';
                    Exit;
                  end;

                FTokenType:= DOCTYPE_PUBID_TOKEN;
                FTokenStart := FInputSource.NextCharInfo;

                // Test for whitespace:
                if not IsXmlWhiteSpaceCodePoint(FInputSource.NextCodePoint) then begin
                  FTokenEnd := FInputSource.CurrentCharInfo;
                  FErrorType := ET_MISSING_WHITE_SPACE;
                  FClue:= ' ';
                  Exit;
                end;

                // Skip whitespace:
                FInputSource.Next;
                while isXmlWhiteSpaceCodePoint(FInputSource.NextCodePoint) do
                  FInputSource.Next;

                FTokenStart := FInputSource.NextCharInfo;

                // Find public identifier:
                if not (FInputSource.NextCodePoint in [DQ_CODE, SQ_CODE]) then begin
                  FTokenEnd := FInputSource.CurrentCharInfo;
                  FErrorType := ET_QUOTATION_MARK_EXPECTED;
                  FClue := '"';
                  Exit;
                end;
                FInputSource.Next;
                FTokenStart := FInputSource.NextCharInfo;
                QuoteCode := FInputSource.CurrentCodePoint;
                while not (FInputSource.NextCodePoint in [QuoteCode, STRING_TERMINATOR_CODE]) do begin
                  FInputSource.Next;
                  FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
                end;
                FTokenEnd := FInputSource.CurrentCharInfo;
                if FInputSource.NextCodePoint <> QuoteCode then begin
                  FErrorType := ET_QUOTATION_MARK_EXPECTED;
                  FClue := WideChar(QuoteCode);
                  Exit;
                end;
                FInputSource.Next;

              end;
            CAPITAL_S_CODE: // 'S' --> 'SYSTEM' found.
              begin
                FInputSource.Next;
                FTokenStart := FInputSource.CurrentCharInfo;
                for I := 0 to 4 do
                  if FInputSource.NextCodePoint = SYSTEM_ID_START[I] then begin
                    FInputSource.Next;
                  end else begin
                    FTokenEnd := FInputSource.CurrentCharInfo;
                    FErrorType := ET_SYSTEM_KEYWORD_EXPECTED;
                    FClue := 'SYSTEM';
                    Exit;
                  end;
                FTokenType := DOCTYPE_PUBID_TOKEN;
                Self.Next;
              end;
            LS_BRACKET_CODE: // '[' found.
              begin
                FTokenType := DOCTYPE_SYSID_TOKEN;
                Self.Next;
              end;
          else
            FInputSource.Next;
            FTokenEnd := FInputSource.CurrentCharInfo;
            FErrorType := ET_SYSTEM_KEYWORD_EXPECTED;
            FClue := 'SYSTEM';
          end; {case ...}

        end;

      DOCTYPE_PUBID_TOKEN:
        begin
          FTokenType:= DOCTYPE_SYSID_TOKEN;
          FTokenStart := FInputSource.NextCharInfo;

          // Test for whitespace:
          if not IsXmlWhiteSpaceCodePoint(FInputSource.NextCodePoint) then begin
            FTokenEnd := FInputSource.CurrentCharInfo;
            FErrorType := ET_MISSING_WHITE_SPACE;
            FClue:= ' ';
            Exit;
          end;

          // Skip whitespace:
          FInputSource.Next;
          while isXmlWhiteSpaceCodePoint(FInputSource.NextCodePoint) do
            FInputSource.Next;

          FTokenStart := FInputSource.NextCharInfo;

          // Find system identifier:
          if not (FInputSource.NextCodePoint in [DQ_CODE, SQ_CODE]) then begin
            FTokenEnd := FInputSource.CurrentCharInfo;
            FErrorType := ET_QUOTATION_MARK_EXPECTED;
            FClue := '"';
            Exit;
          end;
          FInputSource.Next;
          QuoteCode := FInputSource.CurrentCodePoint;
          while not (FInputSource.NextCodePoint in [QuoteCode, STRING_TERMINATOR_CODE]) do begin
            FInputSource.Next;
            FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
          end;
          FTokenEnd := FInputSource.CurrentCharInfo;
          if FInputSource.NextCodePoint <> QuoteCode then begin
            FErrorType := ET_QUOTATION_MARK_EXPECTED;
            FClue := '"';
            Exit;
          end;
          FInputSource.Next;
        end;

      DOCTYPE_START_OF_SOURCE_TOKEN:
        begin
          FTokenType := DOCTYPE_NAME_TOKEN;

          // Test for leading whitespace:
          FInputSource.Next;
          FTokenStart := FInputSource.CurrentCharInfo;
          if not IsXmlWhiteSpaceCodePoint(FInputSource.CurrentCodePoint) then begin
            FTokenEnd := FInputSource.CurrentCharInfo;
            FErrorType := ET_MISSING_WHITE_SPACE;
            FClue:= ' ';
            Exit;
          end;

          // Skip whitespace:
          while isXmlWhiteSpaceCodePoint(FInputSource.NextCodePoint) do
            FInputSource.Next;

          // Find doctype name:
          DoctypeNameStart := True;
          FTokenStart := FInputSource.NextCharInfo;
          while not ( IsXmlWhiteSpaceCodePoint(FInputSource.NextCodePoint) or
                      (FInputSource.NextCodePoint = LS_BRACKET_CODE) or // ['
                      (FInputSource.NextCodePoint = GT_CODE) or         // '>'
                      (FInputSource.NextCodePoint = STRING_TERMINATOR_CODE) ) do begin
            FInputSource.Next;
            FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
            if DoctypeNameStart then begin
              DoctypeNameStart := False;
            end;
          end;
          FTokenEnd := FInputSource.CurrentCharInfo;
        end;

      DOCTYPE_SYSID_TOKEN:
        begin
          // Skip optional whitespace:
          while isXmlWhiteSpaceCodePoint(FInputSource.NextCodePoint) do
            FInputSource.Next;

          case FInputSource.NextCodePoint of
            STRING_TERMINATOR_CODE:
              begin
                FInputSource.Next;
                FTokenEnd := FInputSource.PreviousCharInfo;
                FTokenType := DOCTYPE_END_OF_SOURCE_TOKEN;
              end;
            LS_BRACKET_CODE: // '[' found.
              begin
                FTokenType := DOCTYPE_INTSUBSET_TOKEN;
                FInputSource.Next;
                FTokenStart := FInputSource.NextCharInfo;
                LT_Found := False;
                EM_Found := False;
                CommentStartFound := False;
                CommentEndFound := False;
                InComment := False;
                InPI := False;
                DQ_Open := False;
                SQ_Open := False;
                while FInputSource.NextCodePoint <> STRING_TERMINATOR_CODE do begin
                  FInputSource.Next;
                  if CommentEndFound then begin
                    CommentEndFound := False;
                    InComment := False;
                    if FInputSource.NextCodePoint <> GT_Code then begin
                      FTokenEnd := FInputSource.CurrentCharInfo;
                      FErrorType:= ET_UNCLOSED_COMMENT;
                      FClue:= '-->';
                      Exit;
                    end;
                  end;
                  if InComment then begin
                    if (FInputSource.CurrentCodePoint = HYPHEN_Code) and
                      (FInputSource.NextCodePoint = HYPHEN_Code) then
                      CommentEndFound := True;
                  end;
                  if InPI then begin
                    if (FInputSource.CurrentCodePoint = QM_Code) and
                      (FInputSource.NextCodePoint = GT_Code) then
                      InPI := False;
                  end;
                  if CommentStartFound then begin
                    CommentStartFound := False;
                    if FInputSource.CurrentCodePoint = HYPHEN_CODE then
                      InComment := True;
                  end;
                  if EM_Found then begin
                    EM_Found := False;
                    if FInputSource.CurrentCodePoint = HYPHEN_CODE then
                      CommentStartFound := True;
                  end;
                  if LT_Found then begin
                    LT_Found := False;
                    case FInputSource.CurrentCodePoint of
                      QM_CODE:
                        InPI := True;
                      EM_CODE:
                        EM_Found := True;
                    end;
                  end;
                  if DQ_Open then begin
                    DQ_Open := FInputSource.CurrentCodePoint <> DQ_CODE;
                  end else if SQ_Open then begin
                    SQ_Open := FInputSource.CurrentCodePoint <> SQ_CODE;
                  end else if not (InPI or InComment) then begin
                    case FInputSource.CurrentCodePoint of
                      LT_CODE:         // '<'
                        if not (InPI or InComment) then
                          LT_Found := True;
                      DQ_CODE:         // '"'
                        DQ_Open := True;
                      SQ_CODE:         // '''
                        SQ_Open := True;
                      RS_BRACKET_CODE: // ']'
                        begin
                          FTokenEnd := FInputSource.PreviousCharInfo;
                          Exit;
                        end;
                    end;
                  end;
                  FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
                end;

                if FInputSource.NextCodePoint <> RS_BRACKET_CODE then begin
                  FTokenEnd := FInputSource.CurrentCharInfo;
                  FErrorType := ET_RIGHT_SQUARE_BRACKET_EXPECTED;
                  FClue := ']';
                  Exit;
                end;
                FInputSource.Next;
              end;
          else
            FInputSource.Next;
            FTokenEnd := FInputSource.CurrentCharInfo;
            FErrorType := ET_LEFT_SQUARE_BRACKET_EXPECTED;
            FClue := '[';
          end; {case ...}
        end;
    end;

  except
    on EConvertError do begin
      FTokenEnd := FInputSource.CurrentCharInfo;
      FErrorType:= ET_INVALID_CHARACTER;
    end;
  end; {try ...}
end;



{ TXmlDtdTokenizer }

constructor TXmlDtdTokenizer.create(const aInputSource: TXmlInputSource;
                                    const aTabWidth: cardinal;
                                    const XmlDeclarationAllowed: boolean);
begin
  inherited create(AInputSource, ATabWidth);
  if assigned(AInputSource) then begin
    if XmlDeclarationAllowed then begin
      if aInputSource.hasMalformedDecl
         or not ( aInputSource.declType in [ DT_TEXT_DECLARATION,
                                             DT_XML_OR_TEXT_DECLARATION,
                                             DT_UNSPECIFIED ] )
        then begin
          FTokenType:= DTD_END_OF_SOURCE_TOKEN;
          FErrorType:= ET_INVALID_TEXT_DECL;
        end else if aInputSource.invalidEncoding then begin
          FTokenType:= DTD_END_OF_SOURCE_TOKEN;
          FErrorType:= ET_ENCODING_NOT_SUPPORTED;
        end else begin
          FTokenType:= DTD_START_OF_SOURCE_TOKEN;
          FErrorType:= ET_NONE;
        end;
    end else begin
      if aInputSource.hasMalformedDecl
         or not ( aInputSource.declType in [ DT_UNSPECIFIED ] )
        then begin
          FTokenType:= DTD_END_OF_SOURCE_TOKEN;
          FErrorType:= ET_INVALID_MARKUP_DECL;
        end else begin
          FTokenType:= DTD_START_OF_SOURCE_TOKEN;
          FErrorType:= ET_NONE;
        end;
    end;
  end else FTokenType:= DTD_END_OF_SOURCE_TOKEN;
end;

procedure TXmlDtdTokenizer.next;
const
  EM_CODE          = $21; // code of !
  DQ_CODE          = $22; // code of "
  PERCENT_CODE     = $25; // code of %
  SQ_CODE          = $27; // code of '
  HYPHEN_CODE      = $2D; // code of -
  COLON_CODE       = $3A; // code of :
  SEMICOLON_CODE   = $3B; // code of ;
  LOW_LINE_CODE    = $5F; // code of _
  LT_CODE          = $3C; // code of <
  GT_CODE          = $3E; // code of >
  QM_CODE          = $3F; // code of ?
  CAPITAL_A_CODE   = $41; // code of A
  CAPITAL_E_CODE   = $45; // code of E
  CAPITAL_L_CODE   = $4C; // code of L
  CAPITAL_N_CODE   = $4E; // code of N
  LS_BRACKET_CODE  = $5B; // code of [
  RS_BRACKET_CODE  = $5D; // code of ]
  STRING_TERMINATOR_CODE = $9C;

  ENTITY_DECL_START: array[0..3] of UCS4Char =
    (ord('T'), ord('I'), ord('T'), ord('Y'));
  ELEMENT_DECL_START: array[0..4] of UCS4Char =
    (ord('E'), ord('M'), ord('E'), ord('N'), ord('T'));
  ATTLIST_DECL_START: array[0..5] of UCS4Char =
    (ord('T'), ord('T'), ord('L'), ord('I'), ord('S'), ord('T'));
  NOTATION_DECL_START: array[0..6] of UCS4Char =
    (ord('O'), ord('T'), ord('A'), ord('T'), ord('I'), ord('O'), ord('N'));
var
  i: integer;
  SQ_Open: boolean;
  DQ_Open: boolean;
begin
  if FTokenType = DTD_END_OF_SOURCE_TOKEN then exit;

  FTokenValue.Clear;
  FTokenType:= DTD_INVALID_MARKUP_TOKEN;
  FErrorType:= ET_NONE;
  FClue:= '';
  FTokenStart := FInputSource.NextCharInfo;

  try
    FInputSource.Next;

    case FInputSource.CurrentCodePoint of

      // '<' found:
      LT_CODE: begin
        case FInputSource.NextCodePoint of

          // '?' --> Processing Instruction found:
          QM_CODE: begin
            FTokenType:= DTD_PI_TOKEN;
            FInputSource.Next;
            FTokenStart := FInputSource.NextCharInfo;
            while FInputSource.NextCodePoint <> STRING_TERMINATOR_CODE do begin
              FInputSource.Next;
              if (FInputSource.CurrentCodePoint = QM_CODE) and
                 (FInputSource.NextCodePoint = GT_CODE)
              then begin
                // '?>' found:
                FTokenEnd := FInputSource.PreviousCharInfo;
                FInputSource.Next;
                Exit;
              end;
              FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
            end;
            FTokenEnd := FInputSource.PreviousCharInfo;
            FErrorType:= ET_UNCLOSED_PROCESSING_INSTRUCTION;
            FClue:= '?';
          end;

          // '!' --> Markup declaration found:
          EM_CODE: begin
            FInputSource.Next;
            case FInputSource.NextCodePoint of

              HYPHEN_CODE: begin // '-' --> Comment found:
                FTokenType:= DTD_COMMENT_TOKEN;
                FInputSource.Next;
                if FInputSource.NextCodePoint = HYPHEN_CODE then begin // '<!--' found:
                  FInputSource.Next;
                  FTokenStart := FInputSource.NextCharInfo;
                  while FInputSource.NextCodePoint <> STRING_TERMINATOR_CODE do begin
                    FInputSource.Next;
                    if FInputSource.CurrentCodePoint = HYPHEN_CODE then begin // '-' found
                      FTokenEnd := FInputSource.PreviousCharInfo;
                      FInputSource.Next;
                      case FInputSource.CurrentCodePoint of
                        HYPHEN_CODE: begin // Second '-'
                          if FInputSource.NextCodePoint = GT_CODE then begin// '>'?
                            FInputSource.Next;
                          end else begin
                            FTokenEnd := FInputSource.CurrentCharInfo;
                            FErrorType:= ET_DOUBLE_HYPHEN_IN_COMMENT;
                            FClue:= '>';
                          end;
                          exit;
                        end;
                        STRING_TERMINATOR_CODE: begin
                          FTokenEnd := FInputSource.PreviousCharInfo;
                          FErrorType:= ET_UNCLOSED_COMMENT;
                          FClue:= '-->';
                          exit;
                        end;
                      else
                        // No second '-' --> Add '-' to content of comment:
                        FTokenValue.addUCS4Char(HYPHEN_CODE);
                      end;
                    end;
                    FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
                  end;
                  FTokenEnd := FInputSource.CurrentCharInfo;
                  FErrorType:= ET_UNCLOSED_COMMENT;
                  FClue:= '-->';
                  exit;
                end;
                FTokenEnd := FInputSource.CurrentCharInfo;
                FErrorType:= ET_COMMENT_START_EXPECTED;
                FClue:= '<!--';
              end;

              LS_BRACKET_CODE: begin // '[' --> Conditional Section found:
                FTokenType:= DTD_START_OF_CONDITIONAL_SECTION_TOKEN;
                FInputSource.Next;
                FTokenStart := FInputSource.CurrentCharInfo;
                while not (FInputSource.NextCodePoint in [LS_BRACKET_CODE, RS_BRACKET_CODE]) do begin
                  if FInputSource.NextCodePoint = STRING_TERMINATOR_CODE then begin
                    FTokenEnd := FInputSource.CurrentCharInfo;
                    FErrorType:= ET_INVALID_CONDITIONAL_SECTION;
                    Exit;
                  end;
                  FInputSource.Next;
                  FTokenValue.AddUCS4Char(FInputSource.CurrentCodePoint);
                end;
                FTokenEnd := FInputSource.CurrentCharInfo;
                FInputSource.Next;
              end;

            else // --> Element, Entity, Attlist or Notation Declaration found:
              case FInputSource.NextCodePoint of
                CAPITAL_E_CODE: begin // 'E' --> Element or Entity Declaration found:
                  FInputSource.Next;
                  case FInputSource.NextCodePoint of

                    CAPITAL_L_CODE: begin // 'L' --> Element Declaration found:
                      FTokenType:= DTD_ELEMENT_DECL_TOKEN;
                      FInputSource.Next;
                      for i:= 0 to 4 do
                        if FInputSource.NextCodePoint = ELEMENT_DECL_START[i] then begin
                          FInputSource.Next;
                        end else begin
                          FTokenEnd := FInputSource.CurrentCharInfo;
                          FErrorType:= ET_ELEMENT_DECL_START_EXPECTED;
                          FClue:= '<!ELEMENT';
                          Exit;
                        end;
                    end;

                    CAPITAL_N_CODE: begin // 'N' --> Entity Declaration found:
                      FTokenType:= DTD_ENTITY_DECL_TOKEN;
                      FInputSource.Next;
                      for i:= 0 to 3 do
                        if FInputSource.NextCodePoint = ENTITY_DECL_START[i] then begin
                          FInputSource.Next;
                        end else begin
                          FTokenEnd := FInputSource.CurrentCharInfo;
                          FErrorType:= ET_ENTITY_DECL_START_EXPECTED;
                          FClue:= '<!ENTITY';
                          Exit;
                        end;
                    end;

                  else
                    FTokenEnd := FInputSource.CurrentCharInfo;
                    FErrorType:= ET_INVALID_MARKUP_DECL;
                    Exit;
                  end;
                end;

                CAPITAL_A_CODE: begin // 'A' --> Attribute List Declaration found:
                  FTokenType:= DTD_ATTLIST_DECL_TOKEN;
                  FInputSource.Next;
                  for i:= 0 to 5 do
                    if FInputSource.NextCodePoint = ATTLIST_DECL_START[i] then begin
                      FInputSource.Next;
                    end else begin
                      FTokenEnd := FInputSource.CurrentCharInfo;
                      FErrorType:= ET_ATTLIST_DECL_START_EXPECTED;
                      FClue:= '<!ATTLIST';
                      Exit;
                    end;
                end;

                CAPITAL_N_CODE: begin // 'N' --> Notation Declaration found:
                  FTokenType:= DTD_NOTATION_DECL_TOKEN;
                  FInputSource.Next;
                  for i:= 0 to 6 do
                    if FInputSource.NextCodePoint = NOTATION_DECL_START[i] then begin
                      FInputSource.Next;
                    end else begin
                      FTokenEnd := FInputSource.CurrentCharInfo;
                      FErrorType:= ET_NOTATION_DECL_START_EXPECTED;
                      FClue:= '<!NOTATION';
                      Exit;
                    end;
                end;
              else
                FTokenEnd := FInputSource.CurrentCharInfo;
                FErrorType:= ET_INVALID_MARKUP_DECL;
                Exit;
              end;


              if FInputSource.NextCodePoint = PERCENT_CODE then begin
                // Test for possible parameter entity reference
                FInputSource.Next;
                if not ( IsXmlLetterCodePoint(FInputSource.NextCodePoint) or
                       ( FInputSource.NextCodePoint in [COLON_CODE, LOW_LINE_CODE]) ) then begin
                  while not (FInputSource.NextCodePoint in [LT_CODE, PERCENT_CODE, RS_BRACKET_CODE, STRING_TERMINATOR_CODE]) do
                    FInputSource.Next;  // Fallback to next significant character.
                  FTokenEnd := FInputSource.CurrentCharInfo;
                  FErrorType := ET_WHITESPACE_EXPECTED;
                  case FTokenType of
                    DTD_ELEMENT_DECL_TOKEN:  FClue := '<!ELEMENT ';
                    DTD_ENTITY_DECL_TOKEN:
                      begin
                        FErrorType := ET_INVALID_PARAMETER_ENTITY_DECL;
                        FClue := '<!ENTITY %';
                      end;
                    DTD_ATTLIST_DECL_TOKEN:  FClue := '<!ATTLIST ';
                    DTD_NOTATION_DECL_TOKEN: FClue := '<!NOTATION ';
                  end;
                  Exit;
                end;
                FTokenStart := FInputSource.CurrentCharInfo;
                FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
              end else begin
                if not isXmlWhiteSpaceCodePoint(FInputSource.NextCodePoint) then begin // Test for whitespace
                  while not (FInputSource.NextCodePoint in [LT_CODE, PERCENT_CODE, RS_BRACKET_CODE, STRING_TERMINATOR_CODE]) do
                    FInputSource.Next;  // Fallback to next significant character.
                  FTokenEnd := FInputSource.CurrentCharInfo;
                  FErrorType := ET_WHITESPACE_EXPECTED;
                  case FTokenType of
                    DTD_ELEMENT_DECL_TOKEN:  FClue := '<!ELEMENT ';
                    DTD_ENTITY_DECL_TOKEN:   FClue := '<!ENTITY ';
                    DTD_ATTLIST_DECL_TOKEN:  FClue := '<!ATTLIST ';
                    DTD_NOTATION_DECL_TOKEN: FClue := '<!NOTATION ';
                  end;
                  Exit;
                end;

                // Skip whitespace:
                FInputSource.Next;
                while isXmlWhiteSpaceCodePoint(FInputSource.NextCodePoint) do
                  FInputSource.Next;
                FTokenStart := FInputSource.NextCharInfo;
              end;

              // Determine token value:
              SQ_Open:= False;
              DQ_Open:= False;
              while FInputSource.NextCodePoint <> GT_CODE do begin
                if FInputSource.NextCodePoint = STRING_TERMINATOR_CODE then begin
                  FTokenEnd := FInputSource.CurrentCharInfo;
                  case FTokenType of
                    DTD_ELEMENT_DECL_TOKEN:
                      FErrorType:= ET_UNCLOSED_ELEMENT_DECL;
                    DTD_ENTITY_DECL_TOKEN:
                      FErrorType:= ET_UNCLOSED_ENTITY_DECL;
                    DTD_ATTLIST_DECL_TOKEN:
                      FErrorType:= ET_UNCLOSED_ATTLIST_DECL;
                    DTD_NOTATION_DECL_TOKEN:
                      FErrorType:= ET_UNCLOSED_NOTATION_DECL;
                  end;
                  if SQ_Open then begin
                    FClue:= '''>';
                  end else if DQ_Open then begin
                    FClue:= '">';
                  end else
                    FClue:= '>';
                  Exit;
                end;
                FInputSource.Next;
                FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
                case FInputSource.CurrentCodePoint of
                  SQ_CODE: SQ_Open:= True;
                  DQ_CODE: DQ_Open:= True;
                end;
                while SQ_Open and (FInputSource.NextCodePoint <> STRING_TERMINATOR_CODE) do begin
                  FInputSource.Next;
                  FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
                  if FInputSource.CurrentCodePoint = SQ_CODE then
                    SQ_Open:= False;
                end;
                while DQ_Open and (FInputSource.NextCodePoint <> STRING_TERMINATOR_CODE) do begin
                  FInputSource.Next;
                  FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
                  if FInputSource.CurrentCodePoint = DQ_CODE then
                    DQ_Open:= False;
                end;
              end;
              FTokenEnd := FInputSource.CurrentCharInfo;
              FInputSource.Next;
            end;
          end; {EM_CODE:}

        else
          FTokenEnd := FInputSource.CurrentCharInfo;
          FErrorType:= ET_INVALID_MARKUP_DECL;
          FClue:= '<!';
        end;
      end;

      // Parameter Entity Reference found:
      PERCENT_CODE: begin
        FTokenType:= DTD_PARAMETER_ENTITY_REF_TOKEN;
        FTokenStart := FInputSource.NextCharInfo;
        while not (FInputSource.NextCodePoint in [SEMICOLON_CODE, STRING_TERMINATOR_CODE]) do begin
          FInputSource.Next;
          FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
        end;
        FTokenEnd := FInputSource.CurrentCharInfo;
        if FInputSource.NextCodePoint = STRING_TERMINATOR_CODE then begin
          FErrorType:= ET_UNCLOSED_PARAMETER_ENTITY_REF;
          FClue:= ';';
        end else FInputSource.Next;
      end;

      // End of Conditional Section found:
      RS_BRACKET_CODE: begin
        FTokenType:= DTD_END_OF_CONDITIONAL_SECTION_TOKEN;
        if FInputSource.NextCodePoint = RS_BRACKET_CODE then begin // ']'
          FInputSource.Next;
          if FInputSource.NextCodePoint = GT_CODE then begin // '>'
            FTokenStart := FInputSource.NextCharInfo;
            FInputSource.Next;
          end else begin
            FErrorType:= ET_INVALID_CHARACTER;
            FClue:= ']]>';
          end;
        end else begin
          FErrorType:= ET_INVALID_CHARACTER;
          FClue:= ']]>';
        end;
        FTokenEnd := FInputSource.CurrentCharInfo;
      end;

      // End of source found:
      STRING_TERMINATOR_CODE: begin
        FTokenEnd := FInputSource.CurrentCharInfo;
        FTokenType:= DTD_END_OF_SOURCE_TOKEN;
      end;
    else
      // Whitespace found:
      FTokenType:= DTD_IGNORABLE_WHITESPACE_TOKEN;
      FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
      if not isXmlWhiteSpaceCodePoint(FInputSource.CurrentCodePoint) then begin
        FTokenEnd := FInputSource.CurrentCharInfo;
        FTokenType:= DTD_INVALID_MARKUP_TOKEN;
        FErrorType:= ET_INVALID_MARKUP_DECL;
        Exit;
      end;
      while not (FInputSource.NextCodePoint in [PERCENT_CODE, LT_CODE, RS_BRACKET_CODE, STRING_TERMINATOR_CODE]) do begin
        FInputSource.Next;
        FTokenValue.addUCS4Char(FInputSource.CurrentCodePoint);
        if not isXmlWhiteSpaceCodePoint(FInputSource.CurrentCodePoint) then begin
          FTokenEnd := FInputSource.CurrentCharInfo;
          FTokenType:= DTD_INVALID_MARKUP_TOKEN;
          FErrorType:= ET_INVALID_MARKUP_DECL;
          Exit;
        end;
      end;
      FTokenEnd := FInputSource.CurrentCharInfo;
    end;

  except
    on EConvertError do
    begin
      FTokenEnd := FInputSource.CurrentCharInfo;
      FErrorType:= ET_INVALID_CHARACTER;
    end;
  end; {try ...}
end;

procedure TXmlDtdTokenizer.nextEndOfIgnoredCondSect;
const
  EM_CODE          = $21; // code of !
  LT_CODE          = $3C; // code of <
  GT_CODE          = $3E; // code of >
  LS_BRACKET_CODE  = $5B; // code of [
  RS_BRACKET_CODE  = $5D; // code of ]
  STRING_TERMINATOR_CODE = $9C;
var
  N: Integer;
begin
  if FTokenType = DTD_END_OF_SOURCE_TOKEN then exit;

  FTokenValue.Clear;
  FTokenType := DTD_INVALID_MARKUP_TOKEN;
  FErrorType := ET_NONE;
  FClue := '';
  N := 0;

  try

    while FTokenType = DTD_INVALID_MARKUP_TOKEN do begin

      FInputSource.Next;

      case FInputSource.CurrentCodePoint of

        // '<' found:
        LT_CODE:
          if FInputSource.NextCodePoint = EM_CODE then begin // '!' found?
            FInputSource.Next;
            if FInputSource.NextCodePoint = LS_BRACKET_CODE then begin
              // '<![' --> Conditional section start found:
              FInputSource.Next;
              Inc(N);
            end;
          end;

        // ']' found:
        RS_BRACKET_CODE:
          if FInputSource.NextCodePoint = RS_BRACKET_CODE then begin // ']' found?
            FInputSource.Next;
            while FInputSource.NextCodePoint = RS_BRACKET_CODE do // more ']'?
              FInputSource.Next;
            if FInputSource.NextCodePoint = GT_CODE then begin
              // ']]>' --> Conditional section end found:
              FInputSource.Next;
              if N = 0
                then FTokenType := DTD_END_OF_CONDITIONAL_SECTION_TOKEN
                else Dec(N);
            end;
          end;

        // End of source found:
        STRING_TERMINATOR_CODE:
          FTokenType := DTD_END_OF_SOURCE_TOKEN;
      end;

    end; {while ...}
  except
    on EConvertError do
      FErrorType := ET_INVALID_CHARACTER;
  end; {try ...}
end;

{ TXmlCustomSubsetTokenizer }

constructor TXmlCustomSubsetTokenizer.create(const aInputSource: TXmlInputSource;
                                             const aTabWidth: cardinal;
                                             const aPERepository: TdomPERepository);
begin
  if not assigned(aPERepository)
    then raise ENot_Supported_Err.create('Not supported error.');
  inherited create;
  FPERepository := aPERepository;
  FPEStrStream := nil;
  FPEInputSource := nil;
  FPETokenizer := nil;

  FXmlDtdTokenizer := TXmlDtdTokenizer.create(AInputSource, ATabWidth, GetXmlDeclarationAllowed);
  if FXmlDtdTokenizer.tokenType = DTD_START_OF_SOURCE_TOKEN
    then FTokenType := DTD_ABSTRACT_START_OF_SOURCE_TOKEN
    else FTokenType := DTD_ABSTRACT_END_OF_SOURCE_TOKEN;
  FErrorType := FXmlDtdTokenizer.errorType;  // Value depents on wellformedness of the XML declaration (if any).

  FLocator := FXmlDtdTokenizer;
end;

destructor TXmlCustomSubsetTokenizer.destroy;
begin
  FinalizePETokenizer;
  FXmlDtdTokenizer.Free;
  inherited;
end;

function TXmlCustomSubsetTokenizer.addParameterEntity(const PEName,
                                                            PEData,
                                                            baseUri: wideString): TXmlErrorType;
var
  entityValue: wideString;
  pubId: wideString;
  sysId: wideString;
  NDataDummy: wideString;
  Error: boolean;
begin
  if not IsXmlName(PEName) then begin
    Result := ET_INVALID_ENTITY_NAME;
    Exit;
  end;

  xmlAnalyseEntityDef(PEData, entityValue, sysId, pubId, NDataDummy, Error);
  if Error or (NDataDummy <> '') then begin
    Result:= ET_INVALID_PARAMETER_ENTITY_DECL;
    Exit;
  end;

  try
    if not ((PubId = '') and (SysId = '')) then begin
      if isXmlSystemChars(sysId) and isXmlPubidChars(pubId) then begin
        if PERepository.add(PEName, baseUri, PubId, SysId)
          then Result := ET_NONE
          else Result := ET_DOUBLE_PARAMETER_ENTITY_DECL;
      end else Result := ET_INVALID_PARAMETER_ENTITY_DECL;
    end else begin
      if PERepository.add(PEName, EntityValue)
        then Result := ET_NONE
        else Result := ET_DOUBLE_PARAMETER_ENTITY_DECL;
    end;
  except
    Result := ET_INVALID_PARAMETER_ENTITY_DECL;
  end; {try ...}
end;

procedure TXmlCustomSubsetTokenizer.finalizePETokenizer;
begin
  FLocator := FXmlDtdTokenizer;
  if Assigned(FPETokenizer) then begin
    FPETokenizer.Free;
    FPETokenizer:= nil;
  end;
  if Assigned(FPEInputSource) then begin
    FPEInputSource.Free;
    FPEInputSource:= nil;
  end;
  if Assigned(FPEStrStream) then begin
    FPEStrStream.Free;
    FPEStrStream:= nil;
  end;
end;

function TXmlCustomSubsetTokenizer.getClue: wideString;
begin
  if Assigned(FPETokenizer)
    then result := FPETokenizer.clue
    else result := FClue;
end;

function TXmlCustomSubsetTokenizer.getErrorType: TXmlErrorType;
begin
  if Assigned(FPETokenizer)
    then result := FPETokenizer.errorType
    else result := FErrorType;
end;

function TXmlCustomSubsetTokenizer.getSystemId: wideString;
begin
  if Assigned(FPEInputSource)
    then result := FPEInputSource.SystemId
    else result := FXmlDtdTokenizer.GetUri;
end;

function TXmlCustomSubsetTokenizer.getTabWidth: integer;
begin
  if Assigned(FPETokenizer)
    then result := FPETokenizer.tabWidth
    else result := FXmlDtdTokenizer.TabWidth;
end;

function TXmlCustomSubsetTokenizer.getTokenData: wideString;
begin
  if Assigned(FPETokenizer)
    then result := FPETokenizer.tokenData
    else result := FTokenData;
end;

function TXmlCustomSubsetTokenizer.getTokenName: wideString;
begin
  if Assigned(FPETokenizer)
    then result := FPETokenizer.tokenName
    else result := FTokenName;
end;

function TXmlCustomSubsetTokenizer.getTokenType: TXmlDtdAbstractTokenType;
begin
  if Assigned(FPETokenizer)
    then result := FPETokenizer.tokenType
    else result := FTokenType;
end;

function TXmlCustomSubsetTokenizer.initializePETokenizer(const PEName: wideString;
                                                         const aTabWidth,
                                                               bufSize: integer): TXmlErrorType;
var
  pubId, sysId: wideString;
  S: wideString;
begin
  Assert(FPETokenizer = nil);
  Assert(FPEInputSource = nil);
  Assert(FPEStrStream = nil);

  Result := PERepository.ResolvePE(PEName, S, pubId, sysId);
  if Result <> ET_NONE then exit;
  S:= concat(wideString(#$FEFF), S); // Add byte order mark.
  FPEStrStream:= TUtilsWideStringStream.create(S);
  try
    FPEInputSource:= TXmlInputSource.create(FPEStrStream, PubId, SysId, BufSize,
                       'UTF-8', True, 0, 0, 0, 0, 1);  // xxx implement default encoding? xxx Change offsetFromBeginning parameter?
    FPETokenizer:= TXmlExtSubsetTokenizer.create(FPEInputSource, ATabWidth, PERepository);
    FLocator := FPETokenizer;
    Result:= FPETokenizer.errorType;
  except
    finalizePETokenizer;
    Result:= ET_UNRESOLVABLE_PARAMETER_ENTITY_REFERENCE;
  end;
end;

procedure TXmlCustomSubsetTokenizer.splitNameFromData(const S: wideString;
                                                        out name,
                                                            data: wideString);
var
  i, len: Integer;
  Source: wideString;
begin
  Source:= TrimWhitespace(S);

  // Find Name:
  i:= 1;
  len:= length(Source);
  while i <= len do begin
    if IsXmlWhiteSpace(Source[i]) then break;
    inc(i);
  end;
  Name := copy(Source, 1, i - 1);

  // Find Data:
  while i <= len do begin
    if not IsXmlWhiteSpace(Source[i]) then break;
    inc(i);
  end;
  Data := copy(Source, i, len - i + 1);
end;

function TXmlCustomSubsetTokenizer.translate(const token: TXmlDtdTokenType): TXmlDtdAbstractTokenType;
begin
  // xxx Use a table instead?
  case token of
    DTD_ATTLIST_DECL_TOKEN:                 Result := DTD_ABSTRACT_ATTLIST_DECL_TOKEN;
    DTD_COMMENT_TOKEN:                      Result := DTD_ABSTRACT_COMMENT_TOKEN;
    DTD_ELEMENT_DECL_TOKEN:                 Result := DTD_ABSTRACT_ELEMENT_DECL_TOKEN;
    DTD_END_OF_CONDITIONAL_SECTION_TOKEN:   Result := DTD_ABSTRACT_INVALID_MARKUP_TOKEN;
    DTD_END_OF_SOURCE_TOKEN:                Result := DTD_ABSTRACT_END_OF_SOURCE_TOKEN;
    DTD_ENTITY_DECL_TOKEN:                  Result := DTD_ABSTRACT_INVALID_MARKUP_TOKEN;
    DTD_IGNORABLE_WHITESPACE_TOKEN:         Result := DTD_ABSTRACT_IGNORABLE_WHITESPACE_TOKEN;
    DTD_INVALID_MARKUP_TOKEN:               Result := DTD_ABSTRACT_INVALID_MARKUP_TOKEN;
    DTD_NOTATION_DECL_TOKEN:                Result := DTD_ABSTRACT_NOTATION_DECL_TOKEN;
    DTD_PARAMETER_ENTITY_REF_TOKEN:         Result := DTD_ABSTRACT_PARAMETER_ENTITY_REF_TOKEN;
    DTD_PI_TOKEN:                           Result := DTD_ABSTRACT_PI_TOKEN;
    DTD_START_OF_CONDITIONAL_SECTION_TOKEN: Result := DTD_ABSTRACT_INVALID_MARKUP_TOKEN;
    DTD_START_OF_SOURCE_TOKEN:              Result := DTD_ABSTRACT_START_OF_SOURCE_TOKEN;
  end;
end;

function TXmlCustomSubsetTokenizer.GetEndByteNumber: Int64;
begin
  Result := FLocator.EndByteNumber;
end;

function TXmlCustomSubsetTokenizer.GetEndCharNumber: Int64;
begin
  Result := FLocator.EndCharNumber;
end;

function TXmlCustomSubsetTokenizer.GetEndColumnNumber: Int64;
begin
  Result := FLocator.EndColumnNumber;
end;

function TXmlCustomSubsetTokenizer.GetEndLineNumber: Int64;
begin
  Result := FLocator.EndLineNumber;
end;

function TXmlCustomSubsetTokenizer.GetRelatedASObject: TdomASObject;
begin
  Result := FLocator.RelatedASObject;
end;

function TXmlCustomSubsetTokenizer.GetRelatedNode: TdomNode;
begin
  Result := FLocator.RelatedNode;
end;

function TXmlCustomSubsetTokenizer.GetStartByteNumber: Int64;
begin
  Result := FLocator.StartByteNumber;
end;

function TXmlCustomSubsetTokenizer.GetStartCharNumber: Int64;
begin
  Result := FLocator.StartCharNumber;
end;

function TXmlCustomSubsetTokenizer.GetStartColumnNumber: Int64;
begin
  Result := FLocator.StartColumnNumber;
end;

function TXmlCustomSubsetTokenizer.GetStartLineNumber: Int64;
begin
  Result := FLocator.StartLineNumber;
end;

function TXmlCustomSubsetTokenizer.GetUri: WideString;
begin
  Result := FLocator.Uri;
end;



{ TXmlExtSubsetTokenizer }

constructor TXmlExtSubsetTokenizer.create(const aInputSource: TXmlInputSource;
                                          const aTabWidth: cardinal;
                                          const aPERepository: TdomPERepository);
begin
  inherited;
  FCondSectCount := 0;
end;

function TXmlExtSubsetTokenizer.getXmlDeclarationAllowed: Boolean;
begin
  Result := True;
end;

function TXmlExtSubsetTokenizer.includePERefs(const S: wideString;
                                                var errType: TXmlErrorType): wideString;
const
  NULL:      WideChar = #$00; // End of wideString mark
  PERCENT:   WideChar = #$25; // '%'
  COLON:     WideChar = #$3A; // ':'
  SEMICOLON: WideChar = #$3B; // ';'
  LOW_LINE:  WideChar = #$5F; // '_'
  SPACE_CODE   = $20;
  PERCENT_CODE = $25; // code of %
var
  P : PWideChar;
  resultStr: TUtilsCustomWideStr;
  PEStr: TUtilsCustomWideStr;
  PEValue, PEPubId, PESysId: wideString;
begin
  resultStr:= TUtilsCustomWideStr.Create;
  try
    P:= PWideChar(S);
    while P^ <> NULL do begin
      if P^ = PERCENT then begin
        Inc(P);
        if IsXmlLetter(P^) or (P^ = COLON) or (P^ = LOW_LINE) then begin
          PEStr:= TUtilsCustomWideStr.Create;
          try
            PEStr.AddWideChar(P^);
            Inc(P);
            while IsXmlNameChar(P^) do begin
              PEStr.AddWideChar(P^);
              Inc(P);
            end;
            if P^ = SEMICOLON then begin
              // Include as PE:
              resultStr.AddUCS4Char(SPACE_CODE);
              errType := PERepository.ResolvePE(PEStr.Value, PEValue, PEPubId, PESysId);
              resultStr.AddWideString(PEValue);
              resultStr.AddUCS4Char(SPACE_CODE);
              if errType <> ET_NONE then begin
                result := S;
                exit;
              end;
            end else begin
              resultStr.AddUCS4Char(PERCENT_CODE);
              resultStr.AddWideString(PEStr.Value);
              Dec(P);
            end;
          finally
            PEStr.free;
          end;
        end else begin
          resultStr.AddUCS4Char(PERCENT_CODE);
          Dec(P);
        end;
      end else resultStr.AddWideChar(P^);
      Inc(P);
    end;
    result := resultStr.Value;
  finally
    resultStr.free;
  end;
end;

function TXmlExtSubsetTokenizer.includePERefsInAttrDecl(
  const S: wideString; var errType: TXmlErrorType): wideString;
// Remark: This function does not resolve PERefs in default attribute values.
const
  NULL:      WideChar = #$00; // End of wideString mark
  DQ:        WideChar = #$22; // '"'
  PERCENT:   WideChar = #$25; // '%'
  SQ:        WideChar = #$27; // '''
  COLON:     WideChar = #$3A; // ':'
  SEMICOLON: WideChar = #$3B; // ';'
  LOW_LINE:  WideChar = #$5F; // '_'
  SPACE_CODE   = $20;
  PERCENT_CODE = $25; // code of %
var
  P : PWideChar;
  quoteType: WideChar;
  resultStr: TUtilsCustomWideStr;
  PEStr: TUtilsCustomWideStr;
  PEValue, PEPubId, PESysId: wideString;
begin
  quoteType := #0;
  resultStr:= TUtilsCustomWideStr.Create;
  try
    P:= PWideChar(S);
    while P^ <> NULL do begin
      if (P^ = PERCENT) and (quoteType = #0) then begin
        Inc(P);
        if IsXmlLetter(P^) or (P^ = COLON) or (P^ = LOW_LINE) then begin
          PEStr:= TUtilsCustomWideStr.Create;
          try
            PEStr.AddWideChar(P^);
            Inc(P);
            while IsXmlNameChar(P^) do begin
              PEStr.AddWideChar(P^);
              Inc(P);
            end;
            if P^ = SEMICOLON then begin
              // Include as PE:
              resultStr.AddUCS4Char(SPACE_CODE);
              errType := PERepository.ResolvePE(PEStr.Value, PEValue, PEPubId, PESysId);
              resultStr.AddWideString(PEValue);
              resultStr.AddUCS4Char(SPACE_CODE);
              if errType <> ET_NONE then begin
                result := S;
                exit;
              end;
            end else begin
              resultStr.AddUCS4Char(PERCENT_CODE);
              resultStr.AddWideString(PEStr.Value);
              Dec(P);
            end;
          finally
            PEStr.free;
          end;
        end else begin
          resultStr.AddUCS4Char(PERCENT_CODE);
          Dec(P);
        end;
      end else begin
        resultStr.AddWideChar(P^);
        if P^ = SQ then begin
          if quoteType = SQ then begin
            quoteType := #0;
          end else if quoteType = #0 then begin
            quoteType := SQ;
          end;
        end else if P^ = DQ then begin
          if quoteType = DQ then begin
            quoteType := #0;
          end else if quoteType = #0 then begin
            quoteType := DQ;
          end;
        end;
      end;
      Inc(P);
    end;
    result := resultStr.Value;
  finally
    resultStr.free;
  end;
end;

function TXmlExtSubsetTokenizer.includePERefsInEntityDecl(const S: wideString;
                                                            var errType: TXmlErrorType): wideString;
// Remark: This function does not resolve PERefs in Entity values, or in
//         public or system identifiers.
const
  NULL:      WideChar = #$00; // End of wideString mark
  DQ:        WideChar = #$22; // '"'
  PERCENT:   WideChar = #$25; // '%'
  SQ:        WideChar = #$27; // '''
  COLON:     WideChar = #$3A; // ':'
  SEMICOLON: WideChar = #$3B; // ';'
  LOW_LINE:  WideChar = #$5F; // '_'
  SPACE_CODE   = $20;
  PERCENT_CODE = $25; // code of %
var
  P : PWideChar;
  quoteType: WideChar;
  resultStr: TUtilsCustomWideStr;
  PEStr: TUtilsCustomWideStr;
  PEValue, PEPubId, PESysId: wideString;
begin
  quoteType := #0;
  resultStr:= TUtilsCustomWideStr.Create;
  try
    P:= PWideChar(S);
    while P^ <> NULL do begin
      if P^ = PERCENT then begin
        Inc(P);
        if IsXmlLetter(P^) or (P^ = COLON) or (P^ = LOW_LINE) then begin
          PEStr:= TUtilsCustomWideStr.Create;
          try
            PEStr.AddWideChar(P^);
            Inc(P);
            while IsXmlNameChar(P^) do begin
              PEStr.AddWideChar(P^);
              Inc(P);
            end;
            if P^ = SEMICOLON then begin
              if quoteType = #0 then begin
                // Include as PE:
                resultStr.AddUCS4Char(SPACE_CODE);
                errType := PERepository.ResolvePE(PEStr.Value, PEValue, PEPubId, PESysId);
                resultStr.AddWideString(PEValue);
                resultStr.AddUCS4Char(SPACE_CODE);
              end else begin
                // Include in literal:
                errType := PERepository.ResolvePE(PEStr.Value, PEValue, PEPubId, PESysId);
                resultStr.AddWideString(xmlReplaceQuotes(PEValue));  // Remark: Single or double quotes are first resolved to character references
              end;
              if errType <> ET_NONE then begin
                result := S;
                exit;
              end;
            end else begin
              resultStr.AddUCS4Char(PERCENT_CODE);
              resultStr.AddWideString(PEStr.Value);
              Dec(P);
            end;
          finally
            PEStr.free;
          end;
        end else begin
          resultStr.AddUCS4Char(PERCENT_CODE);
          Dec(P);
        end;
      end else begin
        resultStr.AddWideChar(P^);
        if P^ = SQ then begin
          if quoteType = SQ then begin
            quoteType := #0;
          end else if quoteType = #0 then begin
            quoteType := SQ;
          end;
        end else if P^ = DQ then begin
          if quoteType = DQ then begin
            quoteType := #0;
          end else if quoteType = #0 then begin
            quoteType := DQ;
          end;
        end;
      end;
      Inc(P);
    end;
    result := resultStr.Value;
  finally
    resultStr.free;
  end;
end;

procedure TXmlExtSubsetTokenizer.next;
var
  S: wideString;

  procedure EvaluateEntityDeclaration(const S: wideString;
                                        out ATokenType: TXmlDtdAbstractTokenType;
                                        out ATokenName,
                                            ATokenData: wideString;
                                        out AErrorType: TXmlErrorType);
  const
    PERCENT: WideChar = #$25; // '%'
  var
    i, j, len: Integer;
    Source: wideString;
  begin
    Source:= TrimWhitespace(S);
    len:= length(Source);
    if len = 0 then begin  // No content?
      ATokenType := DTD_ABSTRACT_ENTITY_DECL_TOKEN;
      ATokenName := '';
      ATokenData := '';
      AErrorType := ET_INVALID_ENTITY_DECL;
      Exit;
    end;

    if Source[1] = PERCENT then begin // Parameter Entity?
      ATokenType := DTD_ABSTRACT_PARAMETER_ENTITY_DECL_TOKEN;

      if len = 1 then begin // No content?
        ATokenName := '';
        ATokenData := '';
        AErrorType := ET_INVALID_PARAMETER_ENTITY_DECL;
        Exit;
      end;

      // Skip white space:
      i:= 2;
      while i <= len do begin
        if not IsXmlWhiteSpace(Source[i]) then break;
        inc(i);
      end;

      if IsXmlWhiteSpace(Source[i]) then begin // No content?
        ATokenName := '';
        ATokenData := '';
        AErrorType := ET_INVALID_PARAMETER_ENTITY_DECL;
        Exit;
      end;

      if i = 2 then begin // No whitespace?
        ATokenName := '';
        ATokenData := '';
        AErrorType := ET_WHITESPACE_EXPECTED;
        Exit;
      end;

    end else begin
      ATokenType := DTD_ABSTRACT_ENTITY_DECL_TOKEN;
      i := 1;
    end;

    // Find Name:
    j := i + 1;
    while j <= len do begin
      if IsXmlWhiteSpace(Source[j]) then break;
      inc(j);
    end;
    ATokenName := copy(Source, i, j - i);

    // Skip white space:
    i := j;
    while i <= len do begin
      if not IsXmlWhiteSpace(Source[i]) then break;
      inc(i);
    end;

    ATokenData := copy(Source, i, len - i + 1);

  end;

begin
  if Assigned(FPETokenizer) then begin // Get next token from Parameter Entity?
    FPETokenizer.Next;
    if FPETokenizer.TokenType <> DTD_ABSTRACT_END_OF_SOURCE_TOKEN then Exit;
    FinalizePETokenizer;
  end;

  FXmlDtdTokenizer.Next;

  if FXmlDtdTokenizer.ErrorType <> ET_NONE then begin
    FErrorType := FXmlDtdTokenizer.ErrorType;
    FClue := FXmlDtdTokenizer.Clue;
    FTokenName := '';
    FTokenData := '';
    FTokenType := translate(FXmlDtdTokenizer.TokenType);
  end else begin
    Assert(FXmlDtdTokenizer.TokenType <> DTD_INVALID_MARKUP_TOKEN);
    Assert(FXmlDtdTokenizer.TokenType <> DTD_START_OF_SOURCE_TOKEN);
    FErrorType:= ET_NONE;
    FClue:= '';

    case FXmlDtdTokenizer.TokenType of

      DTD_ATTLIST_DECL_TOKEN: begin
        FTokenType := DTD_ABSTRACT_ATTLIST_DECL_TOKEN;
        SplitNameFromData(IncludePERefsInAttrDecl(FXmlDtdTokenizer.TokenValue, FErrorType),
                          FTokenName, FTokenData);
      end;

      DTD_COMMENT_TOKEN: begin
        FTokenType := DTD_ABSTRACT_COMMENT_TOKEN;
        FTokenName := '';
        FTokenData := FXmlDtdTokenizer.TokenValue;
      end;

      DTD_ELEMENT_DECL_TOKEN: begin
        FTokenType := DTD_ABSTRACT_ELEMENT_DECL_TOKEN;
        SplitNameFromData(IncludePERefs(FXmlDtdTokenizer.TokenValue, FErrorType),
                          FTokenName, FTokenData);
      end;

      DTD_END_OF_CONDITIONAL_SECTION_TOKEN: begin
        if FCondSectCount = 0 then begin
          FErrorType := ET_INVALID_CONDITIONAL_SECTION;
          FTokenType := DTD_ABSTRACT_END_OF_CONDITIONAL_SECTION_TOKEN;
          FTokenName := '';
          FTokenData := '';
        end else begin
          Dec(FCondSectCount);
          Next;
        end;
      end;

      DTD_END_OF_SOURCE_TOKEN: begin
        if FCondSectCount = 0 then begin
          FTokenType := DTD_ABSTRACT_END_OF_SOURCE_TOKEN;
          FTokenName := '';
          FTokenData := '';
        end else begin
          FErrorType := ET_UNCLOSED_CONDITIONAL_SECTION;
          FClue:= ']]>';
          FTokenType := DTD_ABSTRACT_END_OF_SOURCE_TOKEN;
          FTokenName := '';
          FTokenData := '';
        end;
      end;

      DTD_ENTITY_DECL_TOKEN: begin
        S := includePERefsInEntityDecl(FXmlDtdTokenizer.TokenValue, FErrorType);
        if FErrorType in ET_WARNINGS then begin
          EvaluateEntityDeclaration(S, FTokenType, FTokenName, FTokenData, FErrorType);
          if (FTokenType = DTD_ABSTRACT_PARAMETER_ENTITY_DECL_TOKEN) and
             (FErrorType in ET_WARNINGS) then begin
            FErrorType := AddParameterEntity(FTokenName, FTokenData, SystemId);
          end;
        end else begin
          FTokenType := DTD_ABSTRACT_ENTITY_DECL_TOKEN;
          FTokenName := '';
          FTokenData := '';
        end;
      end;

      DTD_IGNORABLE_WHITESPACE_TOKEN: begin
        FTokenType := DTD_ABSTRACT_IGNORABLE_WHITESPACE_TOKEN;
        FTokenName := '';
        FTokenData := FXmlDtdTokenizer.TokenValue;
      end;

      DTD_INVALID_MARKUP_TOKEN: begin
        FTokenType := DTD_ABSTRACT_INVALID_MARKUP_TOKEN;
        FTokenName := '';
        FTokenData := FXmlDtdTokenizer.TokenValue;
      end;

      DTD_NOTATION_DECL_TOKEN: begin
        FTokenType := DTD_ABSTRACT_NOTATION_DECL_TOKEN;
        FTokenName := '';
        SplitNameFromData(IncludePERefs(FXmlDtdTokenizer.TokenValue, FErrorType),
                          FTokenName, FTokenData);
      end;

      DTD_PARAMETER_ENTITY_REF_TOKEN: begin
        FErrorType := InitializePETokenizer(FXmlDtdTokenizer.TokenValue,
                                            FXmlDtdTokenizer.TabWidth,
                                            FXmlDtdTokenizer.FInputSource.bufSize);  // xxx
        if FErrorType <> ET_NONE then begin
          FTokenType := DTD_ABSTRACT_PARAMETER_ENTITY_REF_TOKEN;
          FTokenName := FXmlDtdTokenizer.TokenValue;
          FTokenData := '';
        end else Next;
      end;

      DTD_PI_TOKEN: begin
        FTokenType := DTD_ABSTRACT_PI_TOKEN;
        SplitNameFromData(FXmlDtdTokenizer.TokenValue, FTokenName, FTokenData);
      end;

      DTD_START_OF_CONDITIONAL_SECTION_TOKEN: begin
        S := TrimWhitespace(IncludePERefs(FXmlDtdTokenizer.TokenValue, FErrorType));
        if FErrorType <> ET_NONE then begin
          FTokenType := DTD_ABSTRACT_START_OF_CONDITIONAL_SECTION_TOKEN;
          FTokenName := FXmlDtdTokenizer.TokenValue;
          FTokenData := '';
          Exit;
        end;
        if S = 'INCLUDE' then begin
          Inc(FCondSectCount);
          Next;
        end else if S = 'IGNORE' then begin
          FXmlDtdTokenizer.NextEndOfIgnoredCondSect;
          if FXmlDtdTokenizer.ErrorType <> ET_NONE then begin
            FErrorType := FXmlDtdTokenizer.ErrorType;
            FClue := FXmlDtdTokenizer.Clue;
            FTokenName := '';
            FTokenData := '';
            FTokenType := translate(FXmlDtdTokenizer.TokenType);
          end else if FXmlDtdTokenizer.TokenType = DTD_END_OF_CONDITIONAL_SECTION_TOKEN
            then Next
            else FErrorType := ET_INVALID_CONDITIONAL_SECTION;
        end else FErrorType := ET_INVALID_CONDITIONAL_SECTION;
      end;

      DTD_START_OF_SOURCE_TOKEN: begin
        FTokenType := DTD_ABSTRACT_START_OF_SOURCE_TOKEN;
        FTokenName := '';
        FTokenData := '';
      end;

    end;
  end;
end;



{ TXmlIntSubsetTokenizer }

constructor TXmlIntSubsetTokenizer.create(const aInputSource: TXmlInputSource;
                                          const aTabWidth: cardinal;
                                          const aPERepository: TdomPERepository);
begin
  inherited;
  FResolvePEs := true;
end;

function TXmlIntSubsetTokenizer.getXmlDeclarationAllowed: Boolean;
begin
  Result := False;
end;

procedure TXmlIntSubsetTokenizer.next;

  procedure EvaluateEntityDeclaration(const S: wideString;
                                        out ATokenType: TXmlDtdAbstractTokenType;
                                        out ATokenName,
                                            ATokenData: wideString;
                                        out AErrorType: TXmlErrorType);
  const
    PERCENT: WideChar = #$25; // '%'
  var
    i, j, len: Integer;
    Source: wideString;
  begin
    Source:= TrimWhitespace(S);
    len:= length(Source);
    if len = 0 then begin  // No content?
      ATokenType := DTD_ABSTRACT_ENTITY_DECL_TOKEN;
      ATokenName := '';
      ATokenData := '';
      AErrorType := ET_INVALID_ENTITY_DECL;
      Exit;
    end;

    if Source[1] = PERCENT then begin // Parameter Entity?
      ATokenType := DTD_ABSTRACT_PARAMETER_ENTITY_DECL_TOKEN;

      if len = 1 then begin // No content?
        ATokenName := '';
        ATokenData := '';
        AErrorType := ET_INVALID_PARAMETER_ENTITY_DECL;
        Exit;
      end;

      // Skip white space:
      i:= 2;
      while i <= len do begin
        if not IsXmlWhiteSpace(Source[i]) then break;
        inc(i);
      end;

      if IsXmlWhiteSpace(Source[i]) then begin // No content?
        ATokenName := '';
        ATokenData := '';
        AErrorType := ET_INVALID_PARAMETER_ENTITY_DECL;
        Exit;
      end;

      if i = 2 then begin // No whitespace?
        ATokenName := '';
        ATokenData := '';
        AErrorType := ET_WHITESPACE_EXPECTED;
        Exit;
      end;

    end else begin
      ATokenType := DTD_ABSTRACT_ENTITY_DECL_TOKEN;
      i := 1;
    end;

    // Find Name:
    j := i + 1;
    while j <= len do begin
      if IsXmlWhiteSpace(Source[j]) then break;
      inc(j);
    end;
    ATokenName := copy(Source, i, j - i);

    // Skip white space:
    i := j;
    while i <= len do begin
      if not IsXmlWhiteSpace(Source[i]) then break;
      inc(i);
    end;

    ATokenData := copy(Source, i, len - i + 1);

  end;

begin
  if Assigned(FPETokenizer) then begin // Get next token from Parameter Entity?
    FPETokenizer.Next;
    if FPETokenizer.TokenType <> DTD_ABSTRACT_END_OF_SOURCE_TOKEN then Exit;
    FinalizePETokenizer;
  end;

  FXmlDtdTokenizer.Next;

  if FXmlDtdTokenizer.ErrorType <> ET_NONE then begin
    FErrorType := FXmlDtdTokenizer.ErrorType;
    FClue := FXmlDtdTokenizer.Clue;
    FTokenName := '';
    FTokenData := '';
    FTokenType := translate(FXmlDtdTokenizer.TokenType);
  end else begin
    Assert(FXmlDtdTokenizer.TokenType <> DTD_INVALID_MARKUP_TOKEN);
    Assert(FXmlDtdTokenizer.TokenType <> DTD_START_OF_SOURCE_TOKEN);
    FErrorType:= ET_NONE;
    FClue:= '';

    case FXmlDtdTokenizer.TokenType of

      DTD_ATTLIST_DECL_TOKEN: begin
        FTokenType := DTD_ABSTRACT_ATTLIST_DECL_TOKEN;
        SplitNameFromData(FXmlDtdTokenizer.TokenValue, FTokenName, FTokenData);
      end;

      DTD_COMMENT_TOKEN: begin
        FTokenType := DTD_ABSTRACT_COMMENT_TOKEN;
        FTokenName := '';
        FTokenData := FXmlDtdTokenizer.TokenValue;
      end;

      DTD_ELEMENT_DECL_TOKEN: begin
        FTokenType := DTD_ABSTRACT_ELEMENT_DECL_TOKEN;
        SplitNameFromData(FXmlDtdTokenizer.TokenValue, FTokenName, FTokenData);
      end;

      DTD_END_OF_CONDITIONAL_SECTION_TOKEN: begin
        FErrorType := ET_CONDITIONAL_SECTION_NOT_ALLOWED;
        FTokenType := DTD_ABSTRACT_END_OF_CONDITIONAL_SECTION_TOKEN;
        FTokenName := '';
        FTokenData := FXmlDtdTokenizer.TokenValue;
      end;

      DTD_END_OF_SOURCE_TOKEN: begin
        FTokenType := DTD_ABSTRACT_END_OF_SOURCE_TOKEN;
        FTokenName := '';
        FTokenData := '';
      end;

      DTD_ENTITY_DECL_TOKEN: begin
        EvaluateEntityDeclaration(FXmlDtdTokenizer.TokenValue, FTokenType, FTokenName, FTokenData, FErrorType);
        if (FTokenType = DTD_ABSTRACT_PARAMETER_ENTITY_DECL_TOKEN) and
           (FErrorType in ET_WARNINGS) then begin
          FErrorType := AddParameterEntity(FTokenName, FTokenData, SystemId);
        end;
      end;

      DTD_IGNORABLE_WHITESPACE_TOKEN: begin
        FTokenType := DTD_ABSTRACT_IGNORABLE_WHITESPACE_TOKEN;
        FTokenName := '';
        FTokenData := FXmlDtdTokenizer.TokenValue;
      end;

      DTD_INVALID_MARKUP_TOKEN: begin
        FTokenType := DTD_ABSTRACT_INVALID_MARKUP_TOKEN;
        FTokenName := '';
        FTokenData := FXmlDtdTokenizer.TokenValue;
      end;

      DTD_NOTATION_DECL_TOKEN: begin
        FTokenType := DTD_ABSTRACT_NOTATION_DECL_TOKEN;
        FTokenName := '';
        SplitNameFromData(FXmlDtdTokenizer.TokenValue, FTokenName, FTokenData);
      end;

      DTD_PARAMETER_ENTITY_REF_TOKEN: begin
        if resolvePEs then begin
          FErrorType := InitializePETokenizer(FXmlDtdTokenizer.TokenValue,
                                              FXmlDtdTokenizer.TabWidth,
                                              FXmlDtdTokenizer.FInputSource.bufSize);  // xxx
          if FErrorType <> ET_NONE then begin
            FTokenType := DTD_ABSTRACT_PARAMETER_ENTITY_REF_TOKEN;
            FTokenName := FXmlDtdTokenizer.TokenValue;
            FTokenData := '';
          end else Next;
        end else begin
          FTokenType := DTD_ABSTRACT_PARAMETER_ENTITY_REF_TOKEN;
          FTokenName := FXmlDtdTokenizer.TokenValue;
          FTokenData := '';
        end;
      end;

      DTD_PI_TOKEN: begin
        FTokenType := DTD_ABSTRACT_PI_TOKEN;
        SplitNameFromData(FXmlDtdTokenizer.TokenValue, FTokenName, FTokenData);
      end;

      DTD_START_OF_CONDITIONAL_SECTION_TOKEN: begin
        FErrorType := ET_CONDITIONAL_SECTION_NOT_ALLOWED;
        FTokenType := DTD_ABSTRACT_START_OF_CONDITIONAL_SECTION_TOKEN;
        FTokenName := '';
        FTokenData := FXmlDtdTokenizer.TokenValue;
      end;

      DTD_START_OF_SOURCE_TOKEN: begin
        FTokenType := DTD_ABSTRACT_START_OF_SOURCE_TOKEN;
        FTokenName := '';
        FTokenData := '';
      end;

    end;
  end;
end;



// +++++++++++++++++++++++++ TXmlOutputSource +++++++++++++++++++++++++
constructor TXmlOutputSource.create(const stream: TStream;
                                    const bufSize: integer);
begin
  inherited;
  FCodec := nil;
  SetCodecClass(TUTF8Codec); // Sets also the default LFTranscoding.
end;

destructor TXmlOutputSource.Destroy;
begin
  FCodec.Free;
  inherited;
end;

function TXmlOutputSource.getCodecClass: TUnicodeCodecClass;
begin
  if Assigned(FCodec)
    then Result := TUnicodeCodecClass(FCodec.ClassType)
    else Result := nil;
end;

function TXmlOutputSource.getWriteLFOption: TCodecWriteLFOption;
begin
  result := FCodec.WriteLFOption;
end;

procedure TXmlOutputSource.setCodecClass(const value: TUnicodeCodecClass);
var
  oldWriteLFOption: TCodecWriteLFOption;
begin
  if Assigned(FCodec) then begin
    oldWriteLFOption := FCodec.WriteLFOption;
    FCodec.Free;
  end else oldWriteLFOption := lwCRLF;  // default LFTranscoding.
  if Assigned(Value) then begin
    FCodec := Value.Create;
    FCodec.OnWrite := WriteEventHandler;
    FCodec.WriteLFOption := oldWriteLFOption;
  end else FCodec := nil;
end;

procedure TXmlOutputSource.setWriteLFOption(const value: TCodecWriteLFOption);
begin
  FCodec.WriteLFOption:= value;
end;

procedure TXmlOutputSource.WriteEventHandler(Sender: TObject; const Buf;
  Count: Integer);
begin
  Write(Buf, Count);
end;

procedure TXmlOutputSource.writeUCS4Char(const C: UCS4Char;
                                           out byteCount: integer);
begin
  FCodec.WriteUCS4Char(C, ByteCount);
end;



// ++++++++++++++++++++++++++ TdomError ++++++++++++++++++++++++++
constructor TdomError.create(const ARelatedException: TXmlErrorType;
                             const AStartByteNumber,
                                   AStartCharNumber,
                                   AStartColumnNumber,
                                   AStartLineNumber,
                                   AEndByteNumber,
                                   AEndCharNumber,
                                   AEndColumnNumber,
                                   AEndLineNumber: Int64;
                             const AUri: wideString;
                             const ARelatedASObject: TdomASObject;
                             const ARelatedNode: TdomNode;
                             const ACode: wideString);
begin
  inherited create;

  FRelatedException :=  ARelatedException;

  FStartByteNumber :=   AStartByteNumber;
  FStartCharNumber :=   AStartCharNumber;
  FStartColumnNumber := AStartColumnNumber;
  FStartLineNumber :=   AStartLineNumber;
  FEndByteNumber :=     AEndByteNumber;
  FEndCharNumber :=     AEndCharNumber;
  FEndColumnNumber :=   AEndColumnNumber;
  FEndLineNumber :=     AEndLineNumber;
  FUri :=               AUri;
  FRelatedASObject :=   ARelatedASObject;
  FRelatedNode :=       ARelatedNode;
  FCode :=              code;
end;

constructor TdomError.createFromError(const AError: TdomError);
begin
  if not Assigned(AError) then
    raise ENot_Supported_Err.create('Not supported error.');
  with AError do
    Self.Create(RelatedException, StartByteNumber, StartCharNumber,
          StartColumnNumber, StartLineNumber, EndByteNumber, EndCharNumber,
          EndColumnNumber, EndLineNumber, Uri, RelatedASObject, RelatedNode,
          Code)
end;

constructor TdomError.createFromLocator(const ARelatedException: TXmlErrorType;
                                        const ALocation: IDomLocator;
                                        const ACode: wideString);
begin
  if Assigned(ALocation) then
    with ALocation do
      Self.Create(ARelatedException,  StartByteNumber, StartCharNumber,
          StartColumnNumber, StartLineNumber, EndByteNumber, EndCharNumber,
          EndColumnNumber, EndLineNumber, Uri, RelatedASObject, RelatedNode,
          ACode)
   else Create(ARelatedException, -1, -1, -1, -1, -1, -1, -1, -1, '', nil, nil, ACode);
end;

function TdomError.CloneError: TdomError;
begin
  Result := TDomErrorClass(ClassType).Create(RelatedException, StartByteNumber,
          StartCharNumber, StartColumnNumber, StartLineNumber, EndByteNumber,
          EndCharNumber, EndColumnNumber, EndLineNumber, Uri, RelatedASObject,
          RelatedNode, Code);
end;

function TdomError.GetEndByteNumber: Int64;
begin
  Result := FEndByteNumber;
end;

function TdomError.GetEndCharNumber: Int64;
begin
  Result := FEndCharNumber;
end;

function TdomError.GetEndColumnNumber: Int64;
begin
  Result := FEndColumnNumber;
end;

function TdomError.GetEndLineNumber: Int64;
begin
  Result := FEndLineNumber;
end;

function TdomError.GetRelatedASObject: TdomASObject;
begin
  Result := FRelatedASObject;
end;

function TdomError.GetRelatedNode: TdomNode;
begin
  Result := FRelatedNode;
end;

function TdomError.GetSeverity: TdomSeverity;
begin
  if RelatedException in ET_FATAL_ERRORS
    then result:= DOM_SEVERITY_FATAL_ERROR
  else if RelatedException in ET_ERRORS
    then result:= DOM_SEVERITY_ERROR
  else result:= DOM_SEVERITY_WARNING;
end;

function TdomError.GetStartByteNumber: Int64;
begin
  Result := FStartByteNumber;
end;

function TdomError.GetStartCharNumber: Int64;
begin
  Result := FStartCharNumber;
end;

function TdomError.GetStartColumnNumber: Int64;
begin
  Result := FStartColumnNumber;
end;

function TdomError.GetStartLineNumber: Int64;
begin
  Result := FStartLineNumber;
end;

function TdomError.GetUri: WideString;
begin
  Result := FUri;
end;



// ++++++++++++++++++++++++++++ TdomPERepository +++++++++++++++++++++++++++++
constructor TdomPERepository.create(const aOwner: TXmlCustomReader);
begin
  if not assigned(aOwner) then
    raise EAccessViolation.create('AOwner not specified.');
  inherited create;
  FOwner := aOwner;
  FPEMap:= TdomOwnerNamedNodeMap.create(TdomPEInfoObject);
end;

destructor TdomPERepository.destroy;
begin
  FPEMap.Free;
  inherited;
end;

function TdomPERepository.add(const name,
                                    value: wideString): boolean;
var
  newPEInfoObj: TdomPEInfoObject;
begin
  if not FPEMap.hasNamedItem(name) then begin // Ignore double declarations
    newPEInfoObj:= TdomPEInfoObject.create(self, name, value);
    try
      Result := True;
      FPEMap.add(newPEInfoObj);
    except
      newPEInfoObj.free;
      raise;
    end; {try ...}
  end else Result := False;
end;

function TdomPERepository.add(const name,
                                    baseUri,
                                    pubId,
                                    sysId: wideString): boolean;
var
  newPEInfoObj: TdomPEInfoObject;
begin
  if not FPEMap.hasNamedItem(name) then begin // Ignore double declarations
    newPEInfoObj:= TdomPEInfoObject.createExtParsed(self, name, baseUri, pubId, sysId);
    try
      Result := True;
      FPEMap.add(newPEInfoObj);
    except
      newPEInfoObj.free;
      raise;
    end; {try ...}
  end else Result := False;
end;

procedure TdomPERepository.clear;
begin
  FPEMap.Clear;
end;

function TdomPERepository.getDomImplementation: TDomImplementation;
begin
  Result := FOwner.DOMImpl;
end;

function TdomPERepository.resolvePE(const name: wideString;
                                      out value,
                                          pubId,
                                          sysId: wideString): TXmlErrorType;
begin
  if FPEMap.hasNamedItem(name) then begin
    Result:= ET_NONE;
    try
      with (FPEMap.getNamedItem(name) as TdomPEInfoObject) do begin
        value:= literalValue;
        pubId:= publicId;
        sysId:= entityURI;
      end;
    except
      value:= '';
      pubId:= '';
      sysId:= '';
      Result:= ET_UNRESOLVABLE_PARAMETER_ENTITY_REFERENCE;
    end;
  end else Result:= ET_PARAMETER_ENTITY_DECL_NOT_FOUND;
end;



// ++++++++++++++++++++++ TdomPEInfoObject ++++++++++++++++++++++
constructor TdomPEInfoObject.create(const aOwner: TdomPERepository;
                                                 const entityName,
                                                       litValue: wideString);
begin
  if not assigned(aOwner) then
    raise EAccessViolation.create('AOwner not specified.');
  if not isXmlName(entityName)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  inherited create(nil);
  FRepository:= aOwner;
  FEntityType:= etInternal_Entity;
  FLiteralValue:= resolveCharRefs(litValue);
  FNodeName:= entityName;
  FPublicId:= '';
  FSystemId:= '';
end;

constructor TdomPEInfoObject.createExtParsed(const aOwner: TdomPERepository;
                                                          const entityName,
                                                                aBaseUri,
                                                                pubId,
                                                                sysId: wideString);
begin
  if not assigned(aOwner) then
    raise EAccessViolation.create('AOwner not specified.');
  if not isXmlName(entityName)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  if not isXmlSystemChars(sysId)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  if not isXmlPubidChars(pubId)
    then raise EInvalid_Character_Err.create('Invalid character error.');
  inherited create(nil);
  FBaseUri:= aBaseUri;
  FRepository:= aOwner;
  FEntityType:= etExternal_Entity;
  FLiteralValue:= '';
  FNodeName:= entityName;
  FPublicId:= pubId;
  FSystemId:= sysId;
end;

function TdomPEInfoObject.entityURI: wideString;
var
  Uri: WideString;
begin
  // Calculate absolute system identifier:
  if ResolveRelativeUriWideStr(BaseUri, SystemId, Uri)
    then Result := Uri
    else Result := '';
end;

function TdomPEInfoObject.getLiteralValue: wideString;
var
  Content: TUtilsCustomWideStr;
  InputSrc: TXmlInputSource;
  stream: TStream;
  PId,SId: wideString;
begin
  if entityType = etExternal_Entity then begin
    if not Assigned(repository.DomImplementation) then
      raise EParserException.create('No DomImplementation associated with owner repository.');

    PId:= publicId;
    SId:= systemId;
    stream:= repository.DomImplementation.resolveResource(baseUri,PId,SId);

    // convert external entity value to UTF-16:
    if assigned(stream) then begin
      try
        InputSrc:= TXmlInputSource.Create(Stream, PId, SId, 4096, 'UTF-8',
                     False, 0, 0, 0, 0, 1);  // xxx implement default encoding?  xxx Change offsetFromBeginning parameter?
        try
          with InputSrc do begin
            if HasMalformedDecl
             or not ( DeclType in [ DT_TEXT_DECLARATION,
                                    DT_XML_OR_TEXT_DECLARATION,
                                    DT_UNSPECIFIED] ) then
              raise EParserException.Create('Invalid text declaration.');
            if invalidEncoding then
              raise EParserException.Create('Encoding not supported.');


            Content := TUtilsCustomWideStr.create;
            try
              Next;
              while not Eof do
              begin
                Content.AddUCS4Char(CurrentCodePoint);
                Next;
              end;
              Result := resolveCharRefs(Content.value);
            finally
              Content.Free;
            end;

          end; {with ...}
        finally
          InputSrc.free;
        end; {try}
      finally
        stream.free;
      end; {try ...}
    end else raise EParserException.Create('Unresolveable parameter entity.');

  end else result:= FLiteralValue;
end;

function TdomPEInfoObject.getNodeName: wideString;
begin
  result:= FNodeName;
end;



// +++++++++++++++++++++++++++++++ TXmlSignal +++++++++++++++++++++++++++++++
procedure TXmlSignal.CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                         out Flaw: WideString);
begin
  XmlErrorType := ET_NONE;
  Flaw := '';
end;

function TXmlSignal.CloneSignal(const AReader: TXmlCustomReader): TXmlSignal;
begin
  Result := TXmlSignalClass(ClassType).Create(AReader, StartByteNumber,
              StartCharNumber, StartColumnNumber, StartLineNumber,
              EndByteNumber, EndCharNumber, EndColumnNumber, EndLineNumber, Uri,
              RelatedASObject, RelatedNode);
end;

constructor TXmlSignal.Create(const AReader: TXmlCustomReader;
                              const AStartByteNumber,
                                    AStartCharNumber,
                                    AStartColumnNumber,
                                    AStartLineNumber,
                                    AEndByteNumber,
                                    AEndCharNumber,
                                    AEndColumnNumber,
                                    AEndLineNumber: Int64;
                              const AUri: wideString;
                              const ARelatedASObject: TdomASObject;
                              const ARelatedNode: TdomNode);
begin
  inherited create;

  FReader := AReader;

  FStartByteNumber :=   AStartByteNumber;
  FStartCharNumber :=   AStartCharNumber;
  FStartColumnNumber := AStartColumnNumber;
  FStartLineNumber :=   AStartLineNumber;
  FEndByteNumber :=     AEndByteNumber;
  FEndCharNumber :=     AEndCharNumber;
  FEndColumnNumber :=   AEndColumnNumber;
  FEndLineNumber :=     AEndLineNumber;
  FUri :=               AUri;
  FRelatedASObject :=   ARelatedASObject;
  FRelatedNode :=       ARelatedNode;
end;

constructor TXmlSignal.CreateFromLocator(const AReader: TXmlCustomReader;
                                         const Location: IDomLocator);
begin
  if Assigned(Location)
    then
      with Location do
        Self.Create(AReader, StartByteNumber, StartCharNumber,
          StartColumnNumber, StartLineNumber, EndByteNumber, EndCharNumber,
          EndColumnNumber, EndLineNumber, Uri, RelatedASObject, RelatedNode)
    else
      Create(AReader, -1, -1, -1, -1, -1, -1, -1, -1, '', nil, nil);
end;

function TXmlSignal.GetEndByteNumber: Int64;
begin
  Result := FEndByteNumber;
end;

function TXmlSignal.GetEndCharNumber: Int64;
begin
  Result := FEndCharNumber;
end;

function TXmlSignal.GetEndColumnNumber: Int64;
begin
  Result := FEndColumnNumber;
end;

function TXmlSignal.GetEndLineNumber: Int64;
begin
  Result := FEndLineNumber;
end;

function TXmlSignal.GetRelatedASObject: TdomASObject;
begin
  Result := FRelatedASObject;
end;

function TXmlSignal.GetRelatedNode: TdomNode;
begin
  Result := FRelatedNode;
end;

function TXmlSignal.GetStartByteNumber: Int64;
begin
  Result := FStartByteNumber;
end;

function TXmlSignal.GetStartCharNumber: Int64;
begin
  Result := FStartCharNumber;
end;

function TXmlSignal.GetStartColumnNumber: Int64;
begin
  Result := FStartColumnNumber;
end;

function TXmlSignal.GetStartLineNumber: Int64;
begin
  Result := FStartLineNumber;
end;

function TXmlSignal.GetUri: WideString;
begin
  Result := FUri;
end;

{ TXmlCompletedSignal }

function TXmlCompletedSignal.Scope: TXmlSignalScope;
begin
  Result := [ssDoc, ssDtd];
end;

{ TXmlAbortedSignal }

function TXmlAbortedSignal.Scope: TXmlSignalScope;
begin
  Result := [ssDoc, ssDtd];
end;

{ TXmlCDATASignal }

procedure TXmlCDATASignal.CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                              out Flaw: WideString);
begin
  if IsXmlCData(Data)
    then XmlErrorType := ET_NONE
    else XmlErrorType := ET_INVALID_CDATA_SECTION;
  Flaw := '';
end;

function TXmlCDATASignal.CloneSignal(const AReader: TXmlCustomReader): TXmlSignal;
begin
  Result := inherited CloneSignal(AReader);
  TXmlCDATASignal(Result).Data := Data;
end;

function TXmlCDATASignal.Scope: TXmlSignalScope;
begin
  Result := [ssDoc];
end;

{ TXmlDoctypeSignal }

procedure TXmlDoctypeSignal.CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                                out Flaw: WideString);
begin
  if isXmlName(DoctypeName) and isXmlPubidChars(PublicId) and isXmlSystemChars(SystemId)  then begin
    XmlErrorType := ET_NONE;
    Flaw := '';
  end else begin
    XmlErrorType := ET_INVALID_DOCTYPE;
    Flaw := Data;    // xxx To-do: Be more specific!
  end;
end;

function TXmlDoctypeSignal.CloneSignal(const AReader: TXmlCustomReader): TXmlSignal;
begin
  Result := inherited CloneSignal(AReader);
  TXmlDoctypeSignal(Result).Data := Data;
  TXmlDoctypeSignal(Result).DoctypeName := DoctypeName;
  TXmlDoctypeSignal(Result).IntSubsetStartByteNumber := IntSubsetStartByteNumber;
  TXmlDoctypeSignal(Result).IntSubsetStartCharNumber := IntSubsetStartCharNumber;
  TXmlDoctypeSignal(Result).IntSubsetStartColumn := IntSubsetStartColumn;
  TXmlDoctypeSignal(Result).IntSubsetStartLine := IntSubsetStartLine;
  TXmlDoctypeSignal(Result).PublicId := PublicId;
  TXmlDoctypeSignal(Result).SystemId := SystemId;
end;

function TXmlDoctypeSignal.Scope: TXmlSignalScope;
begin
  Result := [ssDoc];
end;

{ TXmlEndElementSignal }

procedure TXmlEndElementSignal.CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                                   out Flaw: WideString);
begin
  if IsXmlName(TagName) then begin
    XmlErrorType := ET_NONE;
    Flaw := '';
  end else begin
    XmlErrorType := ET_INVALID_ELEMENT_NAME;
    Flaw := TagName;
  end;
end;

function TXmlEndElementSignal.CloneSignal(const AReader: TXmlCustomReader): TXmlSignal;
begin
  Result := inherited CloneSignal(AReader);
  TXmlEndElementSignal(Result).TagName := TagName;
end;

function TXmlEndElementSignal.Scope: TXmlSignalScope;
begin
  Result := [ssDoc];
end;

{ TXmlEndPrefixMappingSignal }

procedure TXmlEndPrefixMappingSignal.CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                                         out Flaw: WideString);
begin
  if IsXmlPrefix(Prefix) or (Prefix = '') then begin
    XmlErrorType := ET_NONE;
    Flaw := '';
  end else begin
    XmlErrorType := ET_INVALID_PREFIX;
    Flaw := Prefix;
  end;
end;

function TXmlEndPrefixMappingSignal.CloneSignal(const AReader: TXmlCustomReader): TXmlSignal;
begin
  Result := inherited CloneSignal(AReader);
  TXmlEndPrefixMappingSignal(Result).Prefix := Prefix;
end;

function TXmlEndPrefixMappingSignal.Scope: TXmlSignalScope;
begin
  Result := [ssDoc];
end;

{ TXmlEntityRefSignal }

procedure TXmlEntityRefSignal.CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                                  out Flaw: WideString);
begin
  if IsXmlName(EntityName) then begin
    XmlErrorType := ET_NONE;
    Flaw := '';
  end else begin
    XmlErrorType := ET_INVALID_ENTITY_NAME;
    Flaw := EntityName;
  end;
end;

function TXmlEntityRefSignal.CloneSignal(const AReader: TXmlCustomReader): TXmlSignal;
begin
  Result := inherited CloneSignal(AReader);
  TXmlEntityRefSignal(Result).EntityName := EntityName;
end;

function TXmlEntityRefSignal.Scope: TXmlSignalScope;
begin
  Result := [ssDoc];
end;

{ TXmlPCDATASignal }

constructor TXmlPCDATASignal.Create(const AReader: TXmlCustomReader;
  const AStartByteNumber, AStartCharNumber, AStartColumnNumber,
  AStartLineNumber, AEndByteNumber, AEndCharNumber, AEndColumnNumber,
  AEndLineNumber: Int64; const AUri: wideString;
  const ARelatedASObject: TdomASObject; const ARelatedNode: TdomNode);
begin
  inherited;
  FCharRefGenerated := False;
end;

procedure TXmlPCDATASignal.CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                               out Flaw: WideString);
begin
  if IsXmlCData(Data) then begin
    XmlErrorType := ET_NONE;
    Flaw := '';
  end else begin
    XmlErrorType := ET_INVALID_CHARACTER;
    Flaw := Data; // xxx To-do: Be more specific.
  end;
end;

function TXmlPCDATASignal.CloneSignal(const AReader: TXmlCustomReader): TXmlSignal;
begin
  Result := inherited CloneSignal(AReader);
  TXmlPCDATASignal(Result).Data := Data;
  TXmlPCDATASignal(Result).CharRefGenerated := CharRefGenerated;
end;

function TXmlPCDATASignal.Scope: TXmlSignalScope;
begin
  Result := [ssDoc];
end;

{ TXmlSkippedEntitySignal }

function TXmlSkippedEntitySignal.CloneSignal(const AReader: TXmlCustomReader): TXmlSignal;
begin
  Result := inherited CloneSignal(AReader);
  TXmlSkippedEntitySignal(Result).EntityName := EntityName;
end;

function TXmlSkippedEntitySignal.Scope: TXmlSignalScope;
begin
  Result := [ssDoc];
end;

{ TXmlStartDocumentSignal }

procedure TXmlStartDocumentSignal.CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                                      out Flaw: WideString);
begin
  if ( IsXmlEncName(EncodingName) or (EncodingName = '') ) and
     ( IsXmlVersionNum(Version) or (Version = '') ) then begin
    XmlErrorType := ET_NONE;
    Flaw := '';
  end else begin
    XmlErrorType := ET_INVALID_XML_DECL;
    Flaw := ''; // xxx To-do: Be more specific.
  end;
end;

function TXmlStartDocumentSignal.CloneSignal(const AReader: TXmlCustomReader): TXmlSignal;
begin
  Result := inherited CloneSignal(AReader);
  TXmlStartDocumentSignal(Result).EncodingName := EncodingName;
  TXmlStartDocumentSignal(Result).InputEncoding := InputEncoding;
  TXmlStartDocumentSignal(Result).StandaloneDecl := StandaloneDecl;
  TXmlStartDocumentSignal(Result).Version := Version;
end;

function TXmlStartDocumentSignal.Scope: TXmlSignalScope;
begin
  Result := [ssDoc];
end;

{ TXmlStartDocumentFragmentSignal }

procedure TXmlStartDocumentFragmentSignal.CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                                              out Flaw: WideString);
begin
  if IsXmlEncName(EncodingName) or (EncodingName = '') then begin
    XmlErrorType := ET_NONE;
    Flaw := '';
  end else begin
    XmlErrorType := ET_INVALID_XML_DECL;
    Flaw := ''; // xxx To-do: Be more specific.
  end;
end;

function TXmlStartDocumentFragmentSignal.CloneSignal(const AReader: TXmlCustomReader): TXmlSignal;
begin
  Result := inherited CloneSignal(AReader);
  TXmlStartDocumentFragmentSignal(Result).EncodingName := EncodingName;
end;

function TXmlStartDocumentFragmentSignal.Scope: TXmlSignalScope;
begin
  Result := [ssDoc];
end;

{ TXmlStartElementSignal }

procedure TXmlStartElementSignal.CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                                     out Flaw: WideString);
var
  AttName: WideString;
  AttValue: WideString;
  CharacRef: WideString;
  I, J: Integer;
  InEntity: Boolean;
  Text: WideString;
  V: wideString;
begin
  if not IsXmlName(TagName) then begin
    XmlErrorType := ET_INVALID_ELEMENT_NAME;
    Flaw := TagName;
  end else begin
    for I := 0 to Pred(Attributes.Length) do begin
      AttName := Attributes.Names[I];
      AttValue := Attributes.Values[I];

      if Attributes.IndexOfName(AttName) <> I then begin
        XmlErrorType := ET_DOUBLE_ATTRIBUTE_NAME;
        Flaw := AttName;
        Exit;
      end;

      if not IsXmlName(AttName) then begin
        XmlErrorType := ET_INVALID_ATTRIBUTE_NAME;
        Flaw := AttName;
        Exit;
      end;

      if Pos('&', AttValue) = 0 then begin
        if not IsXmlCharData(AttValue) then begin
          XmlErrorType := ET_INVALID_ATTRIBUTE_VALUE;
          Flaw := AttValue;
          Exit;
        end;
      end else begin
        InEntity:= False;
        Text := '';
        for J := 1 to Length(AttValue) do begin
          if InEntity then begin
            if AttValue[J] = ';' then begin
              if Text[1] = '#' then begin // CharRef
                try
                  CharacRef := Concat(WideString('&'), Text, WideString(';'));
                  V := XmlCharRefToStr(CharacRef);
                except
                  on EConvertError do begin
                    XmlErrorType := ET_INVALID_CHARREF;
                    Flaw := CharacRef;
                    Exit;
                  end;
                end; {try}
              end else begin  // EntityRef
                if not IsXmlName(Text) then begin
                  XmlErrorType := ET_INVALID_ENTITY_NAME;
                  Flaw := Text;
                  Exit;
                end;
              end;
              Text := '';
              InEntity := False;
            end else Text:= Concat(Text, WideString(AttValue[J]));
          end else begin
            if AttValue[J] = '&' then begin
              InEntity := True;
            end else if (AttValue[J] = '<') or not IsXmlChar(AttValue[J]) then begin
              XmlErrorType := ET_INVALID_ATTRIBUTE_VALUE;
              Flaw := AttValue[J];
              Exit;
            end;
          end; {if ...}
        end; {for J ...}

        // invalid attribute value?
        if InEntity then begin
          XmlErrorType := ET_INVALID_ATTRIBUTE_VALUE;  // xxx To-do: Be more specific (Character or Entity reference not closed)
          Flaw := AttValue;
          Exit;
        end; {if ...}

      end; {if ...}
    end; {for I ...}
    XmlErrorType := ET_NONE;
    Flaw := '';
  end; {if ... else ...}
end;

constructor TXmlStartElementSignal.Create(const AReader: TXmlCustomReader;
                                          const AStartByteNumber,
                                                AStartCharNumber,
                                                AStartColumnNumber,
                                                AStartLineNumber,
                                                AEndByteNumber,
                                                AEndCharNumber,
                                                AEndColumnNumber,
                                                AEndLineNumber: Int64;
                                          const AUri: wideString;
                                          const ARelatedASObject: TdomASObject;
                                          const ARelatedNode: TdomNode);
begin
  inherited;
  FAttributes := TUtilsNameValueList.Create;
end;

function TXmlStartElementSignal.CloneSignal(const AReader: TXmlCustomReader): TXmlSignal;
begin
  Result := inherited CloneSignal(AReader);
  TXmlStartElementSignal(Result).Attributes := Attributes;
  TXmlStartElementSignal(Result).TagName := TagName;
end;

destructor TXmlStartElementSignal.Destroy;
begin
  FAttributes.Free;
  inherited;
end;

function TXmlStartElementSignal.Scope: TXmlSignalScope;
begin
  Result := [ssDoc];
end;

procedure TXmlStartElementSignal.SetAttributes(const Value: TUtilsNameValueList);
begin
  FAttributes.Assign(Value);
end;

{ TXmlStartPrefixMappingSignal }

procedure TXmlStartPrefixMappingSignal.CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                                           out Flaw: WideString);
const
  SQ: WideString = #39; // code of '
  DQ: WideString = #34; // code of "
begin
  if ( (Prefix = 'xmlns') and (Uri <> 'http://www.w3.org/2000/xmlns/') )
    or ( (Prefix <> '') and not isXmlPrefix(Prefix) ) then begin
    XmlErrorType := ET_INVALID_PREFIX;
    Flaw := Prefix;
  end else if not IsUriURI_referenceWideStr(Uri) then begin
    XmlErrorType := ET_INVALID_NAMESPACE_URI;
    Flaw := Uri;
  end else begin
    XmlErrorType := ET_NONE;
    Flaw := '';
  end;
end;

function TXmlStartPrefixMappingSignal.CloneSignal(const AReader: TXmlCustomReader): TXmlSignal;
begin
  Result := inherited CloneSignal(AReader);
  TXmlStartPrefixMappingSignal(Result).Prefix := Prefix;
  TXmlStartPrefixMappingSignal(Result).Uri := Uri;
end;

function TXmlStartPrefixMappingSignal.Scope: TXmlSignalScope;
begin
  Result := [ssDoc];
end;

{ TXmlCommentSignal }

procedure TXmlCommentSignal.CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                                out Flaw: WideString);
const
  HYPHEN: WideChar = #$2D; // Flaw of -
begin
  if Data <> '' then begin
    if Pos('--', Data) > 0 then begin
      XmlErrorType := ET_DOUBLE_HYPHEN_IN_COMMENT;
      Flaw := '--';
      Exit;
    end else if Data[Length(Data)] = HYPHEN then begin
      XmlErrorType := ET_HYPHEN_AT_COMMENT_END;
      Flaw := '-';
      Exit;
    end else if not IsXmlChars(Data) then begin
      XmlErrorType := ET_INVALID_CHARACTER;
      Flaw := Data; // xxx To-do: make it more specific!
      Exit;
    end;
  end;
  XmlErrorType := ET_NONE;
  Flaw := '';
end;

function TXmlCommentSignal.CloneSignal(const AReader: TXmlCustomReader): TXmlSignal;
begin
  Result := inherited CloneSignal(AReader);
  TXmlCommentSignal(Result).Data := Data;
end;

function TXmlCommentSignal.Scope: TXmlSignalScope;
begin
  Result := [ssDoc, ssDtd];
end;

{ TXmlProcessingInstructionSignal }

procedure TXmlProcessingInstructionSignal.CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                                              out Flaw: WideString);
begin
  if not IsXmlPITarget(Target) then begin
    XmlErrorType := ET_INVALID_PROCESSING_INSTRUCTION;
    Flaw := Target;
  end else if pos('?>', Data) > 0 then begin
    XmlErrorType := ET_INVALID_PROCESSING_INSTRUCTION;
    Flaw := '?>';
  end else if not IsXmlChars(Data) then begin
    XmlErrorType := ET_INVALID_CHARACTER;
    Flaw := Data;  // xxx To-do: Be more specific.
  end else begin
    XmlErrorType := ET_NONE;
    Flaw := '';
  end;
end;

function TXmlProcessingInstructionSignal.CloneSignal(const AReader: TXmlCustomReader): TXmlSignal;
begin
  Result := inherited CloneSignal(AReader);
  TXmlProcessingInstructionSignal(Result).Data := Data;
  TXmlProcessingInstructionSignal(Result).Target := Target;
end;

function TXmlProcessingInstructionSignal.Scope: TXmlSignalScope;
begin
  Result := [ssDoc, ssDtd];
end;

{ TXmlAttributeDefinitionSignal }

procedure TXmlAttributeDefinitionSignal.CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                                            out Flaw: WideString);
const
  LT: WideChar  = #60;  // '<'
var
  CharacRef: WideString;
  I, J: Integer;
  InEntity: Boolean;
  Text: WideString;
  V: wideString;
begin
  if not IsXmlName(AttributeName) then begin
    XmlErrorType := ET_INVALID_ATTRIBUTE_DECL; // xxx To-do: Be more specific.
    Flaw := AttributeName;
  end else if not IsXmlName(ElementName) then begin
    XmlErrorType := ET_INVALID_ATTRIBUTE_DECL;  // xxx To-do: Be more specific.
    Flaw := ElementName;
  end else if Pos(LT, DefaultValue) > 0 then begin
    XmlErrorType := ET_INVALID_ATTRIBUTE_DECL; // xxx To-do: Be more specific.
    Flaw := DefaultValue;
  end else begin
    if AttributeType = AS_NOTATION_DATATYPE then begin
      for I := 0 to Pred(Enumeration.Count) do
        if not IsXmlName(Enumeration[I]) then begin
          XmlErrorType := ET_INVALID_ATTRIBUTE_DECL; // xxx To-do: Be more specific.
          Flaw := Enumeration[I];
          Exit;
        end;
    end else begin
      for I := 0 to Pred(Enumeration.Count) do
        if not IsXmlNmtoken(Enumeration[I]) then begin
          XmlErrorType := ET_INVALID_ATTRIBUTE_DECL; // xxx To-do: Be more specific.
          Flaw := Enumeration[I];
          Exit;
        end;
    end;

    // Check default value:
    if Pos('&', DefaultValue) = 0 then begin
      if not IsXmlCharData(DefaultValue) then begin
        XmlErrorType := ET_INVALID_ATTRIBUTE_VALUE;
        Flaw := DefaultValue;
        Exit;
      end;
    end else begin
      InEntity:= False;
      Text := '';
      for J := 1 to Length(DefaultValue) do begin
        if InEntity then begin
          if DefaultValue[J] = ';' then begin
            if Text[1] = '#' then begin // CharRef
              try
                CharacRef := Concat(WideString('&'), Text, WideString(';'));
                V := XmlCharRefToStr(CharacRef);
              except
                on EConvertError do begin
                  XmlErrorType := ET_INVALID_CHARREF;
                  Flaw := CharacRef;
                  Exit;
                end;
              end; {try}
            end else begin  // EntityRef
              if not IsXmlName(Text) then begin
                XmlErrorType := ET_INVALID_ENTITY_NAME;
                Flaw := Text;
                Exit;
              end;
            end;
            Text := '';
            InEntity := False;
          end else Text:= Concat(Text, WideString(DefaultValue[J]));
        end else begin
          if DefaultValue[J] = '&' then begin
            InEntity := True;
          end else if (DefaultValue[J] = '<') or not IsXmlChar(DefaultValue[J]) then begin
            // WFC: No < in Attribute Values (XML 1.0, § 3.3.2), etc.
            XmlErrorType := ET_INVALID_ATTRIBUTE_VALUE;
            Flaw := DefaultValue[J];
            Exit;
          end;
        end; {if ...}
      end; {for J ...}

      // Invalid attribute value?
      if InEntity then begin
        XmlErrorType := ET_INVALID_ATTRIBUTE_VALUE;  // xxx To-do: Be more specific (Character or Entity reference not closed)
        Flaw := DefaultValue;
        Exit;
      end; {if ...}

    end; {if ...}

    XmlErrorType := ET_NONE;
    Flaw := '';
  end;
end;

function TXmlAttributeDefinitionSignal.CloneSignal(const AReader: TXmlCustomReader): TXmlSignal;
begin
  Result := inherited CloneSignal(AReader);
  TXmlAttributeDefinitionSignal(Result).AttributeName := AttributeName;
  TXmlAttributeDefinitionSignal(Result).AttributeType := AttributeType;
  TXmlAttributeDefinitionSignal(Result).Constraint := Constraint;
  TXmlAttributeDefinitionSignal(Result).DefaultValue := DefaultValue;
  TXmlAttributeDefinitionSignal(Result).ElementName := ElementName;
  TXmlAttributeDefinitionSignal(Result).Enumeration := Enumeration;
end;

constructor TXmlAttributeDefinitionSignal.Create(const AReader: TXmlCustomReader;
                                                 const AStartByteNumber,
                                                       AStartCharNumber,
                                                       AStartColumnNumber,
                                                       AStartLineNumber,
                                                       AEndByteNumber,
                                                       AEndCharNumber,
                                                       AEndColumnNumber,
                                                       AEndLineNumber: Int64;
                                                 const AUri: wideString;
                                                 const ARelatedASObject: TdomASObject;
                                                 const ARelatedNode: TdomNode);
begin
  inherited;
  FEnumeration := TUtilsWideStringList.Create;
end;

destructor TXmlAttributeDefinitionSignal.Destroy;
begin
  FEnumeration.Free;
  inherited;
end;

function TXmlAttributeDefinitionSignal.Scope: TXmlSignalScope;
begin
  Result := [ssDtd];
end;

procedure TXmlAttributeDefinitionSignal.SetEnumeration(const Value: TUtilsWideStringList);
begin
  FEnumeration.Assign(Value);
end;

{ TXmlElementTypeDeclarationSignal }

procedure TXmlElementTypeDeclarationSignal.CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                                               out Flaw: WideString);
begin
  if IsXmlName(ElementName) then begin
    XmlErrorType := ET_NONE;
    Flaw := '';
  end else begin
    XmlErrorType := ET_INVALID_ELEMENT_DECL;  // xxx To-do: Be more specific.
    Flaw := ElementName;
  end;
end;

function TXmlElementTypeDeclarationSignal.CloneSignal(const AReader: TXmlCustomReader): TXmlSignal;
begin
  Result := inherited CloneSignal(AReader);
  TXmlElementTypeDeclarationSignal(Result).Data := Data;
  TXmlElementTypeDeclarationSignal(Result).ElementName := ElementName;
end;

function TXmlElementTypeDeclarationSignal.Scope: TXmlSignalScope;
begin
  Result := [ssDtd];
end;

{ TXmlEntityDeclarationSignal }

procedure TXmlEntityDeclarationSignal.CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                                          out Flaw: WideString);
begin
  if not IsXmlName(EntityName) then begin
    XmlErrorType := ET_INVALID_ENTITY_DECL;  // xxx To-do: Be more specific.
    Flaw := EntityName;
    Exit;
  end;
  if EntityValue <> '' then begin
    if not isXmlEntityValueChars(EntityValue) then begin
      XmlErrorType := ET_INVALID_ENTITY_DECL;  // xxx To-do: Be more specific.
      Flaw := EntityValue;
      Exit;
    end;
    if not ( (PublicId = '') and
             (SystemId = '') and
             (NotationName = '') ) then begin
      XmlErrorType := ET_INVALID_ENTITY_DECL;  // xxx To-do: Be more specific.
      Flaw := '';
      Exit;
    end;
  end;
  if not isXmlSystemChars(SystemId) then begin
    XmlErrorType := ET_INVALID_ENTITY_DECL;  // xxx To-do: Be more specific.
    Flaw := SystemId;
    Exit;
  end;
  if not isXmlPubidChars(PublicId) then begin
    XmlErrorType := ET_INVALID_ENTITY_DECL;  // xxx To-do: Be more specific.
    Flaw := PublicId;
    Exit;
  end;
  if (NotationName <> '') and (not isXmlName(NotationName)) then begin
    XmlErrorType := ET_INVALID_ENTITY_DECL;  // xxx To-do: Be more specific.
    Flaw := NotationName;
    Exit;
  end;

  XmlErrorType := ET_NONE;
  Flaw := '';
end;

function TXmlEntityDeclarationSignal.CloneSignal(const AReader: TXmlCustomReader): TXmlSignal;
begin
  Result := inherited CloneSignal(AReader);
  TXmlEntityDeclarationSignal(Result).EntityName := EntityName;
  TXmlEntityDeclarationSignal(Result).EntityValue := EntityValue;
  TXmlEntityDeclarationSignal(Result).NotationName := NotationName;
  TXmlEntityDeclarationSignal(Result).PublicId := PublicId;
  TXmlEntityDeclarationSignal(Result).SystemId := SystemId;
end;

function TXmlEntityDeclarationSignal.Scope: TXmlSignalScope;
begin
  Result := [ssDtd];
end;

{ TXmlNotationDeclarationSignal }

procedure TXmlNotationDeclarationSignal.CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                                            out Flaw: WideString);
begin
  if not IsXmlName(NotationName) then begin
    XmlErrorType := ET_INVALID_NOTATION_DECL;  // xxx To-do: Be more specific.
    Flaw := NotationName;
  end else if not isXmlSystemChars(SystemId) then begin
    XmlErrorType := ET_INVALID_NOTATION_DECL;  // xxx To-do: Be more specific.
    Flaw := SystemId;
  end else if not isXmlPubidChars(PublicId) then begin
    XmlErrorType := ET_INVALID_NOTATION_DECL;  // xxx To-do: Be more specific.
    Flaw := PublicId;
  end else begin
    XmlErrorType := ET_NONE;
    Flaw := '';
  end;
end;

function TXmlNotationDeclarationSignal.CloneSignal(const AReader: TXmlCustomReader): TXmlSignal;
begin
  Result := inherited CloneSignal(AReader);
  TXmlNotationDeclarationSignal(Result).NotationName := NotationName;
  TXmlNotationDeclarationSignal(Result).PublicId := PublicId;
  TXmlNotationDeclarationSignal(Result).SystemId := SystemId;
end;

function TXmlNotationDeclarationSignal.Scope: TXmlSignalScope;
begin
  Result := [ssDtd];
end;

{ TXmlParameterEntityDeclarationSignal }

procedure TXmlParameterEntityDeclarationSignal.CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                                                   out Flaw: WideString);
begin
  if not IsXmlName(EntityName) then begin
    XmlErrorType := ET_INVALID_ENTITY_DECL;  // xxx To-do: Be more specific.
    Flaw := EntityName;
    Exit;
  end;
  if EntityValue <> '' then begin
    if not isXmlEntityValueChars(EntityValue) then begin
      XmlErrorType := ET_INVALID_ENTITY_DECL;  // xxx To-do: Be more specific.
      Flaw := EntityValue;
      Exit;
    end;
    if not ( (PublicId = '') and
             (SystemId = '') ) then begin
      XmlErrorType := ET_INVALID_ENTITY_DECL;  // xxx To-do: Be more specific.
      Flaw := '';
      Exit;
    end;
  end;
  if not isXmlSystemChars(SystemId) then begin
    XmlErrorType := ET_INVALID_ENTITY_DECL;  // xxx To-do: Be more specific.
    Flaw := SystemId;
    Exit;
  end;
  if not isXmlPubidChars(PublicId) then begin
    XmlErrorType := ET_INVALID_ENTITY_DECL;  // xxx To-do: Be more specific.
    Flaw := PublicId;
    Exit;
  end;

  XmlErrorType := ET_NONE;
  Flaw := '';
end;

function TXmlParameterEntityDeclarationSignal.CloneSignal(const AReader: TXmlCustomReader): TXmlSignal;
begin
  Result := inherited CloneSignal(AReader);
  TXmlParameterEntityDeclarationSignal(Result).EntityName := EntityName;
  TXmlParameterEntityDeclarationSignal(Result).EntityValue := EntityValue;
  TXmlParameterEntityDeclarationSignal(Result).PublicId := PublicId;
  TXmlParameterEntityDeclarationSignal(Result).SystemId := SystemId;
end;

function TXmlParameterEntityDeclarationSignal.Scope: TXmlSignalScope;
begin
  Result := [ssDtd];
end;

{ TXmlStartExtDtdSignal }

procedure TXmlStartExtDtdSignal.CheckWellformedness(out XmlErrorType: TXmlErrorType;
                                                    out Flaw: WideString);
begin
  if ( IsXmlEncName(EncodingName) or (EncodingName = '') ) and
     ( IsXmlVersionNum(Version)   or (Version = '') ) then begin
    XmlErrorType := ET_NONE;
    Flaw := '';
  end else begin
    XmlErrorType := ET_INVALID_TEXT_DECL;  // xxx To-do: Be more specific.
    Flaw := '';
  end;
end;

function TXmlStartExtDtdSignal.CloneSignal(const AReader: TXmlCustomReader): TXmlSignal;
begin
  Result := inherited CloneSignal(AReader);
  TXmlStartExtDtdSignal(Result).EncodingName := EncodingName;
  TXmlStartExtDtdSignal(Result).InputEncoding := InputEncoding;
  TXmlStartExtDtdSignal(Result).PublicId := PublicId;
  TXmlStartExtDtdSignal(Result).SystemId := SystemId;
  TXmlStartExtDtdSignal(Result).Version := Version;
end;

function TXmlStartExtDtdSignal.Scope: TXmlSignalScope;
begin
  Result := [ssDtd];
end;

{ TXmlStartIntDtdSignal }

function TXmlStartIntDtdSignal.CloneSignal(const AReader: TXmlCustomReader): TXmlSignal;
begin
  Result := inherited CloneSignal(AReader);
  TXmlStartIntDtdSignal(Result).SystemId := SystemId;
end;

function TXmlStartIntDtdSignal.Scope: TXmlSignalScope;
begin
  Result := [ssDtd];
end;



// ++++++++++++++++++++++++++++ TXmlCustomHandler ++++++++++++++++++++++++++++
procedure TXmlCustomHandler.sendErrorNotification(const target: TXmlCustomReader;
                                                  const xmlErrorType: TXmlErrorType;
                                                  const location: IDomLocator;
                                                  const code: wideString);
begin
  if Assigned(Target) then begin
    Target.SendErrorNotification(XmlErrorType, Location, Code);
  end else if xmlErrorType in ET_FATAL_ERRORS then begin
    raise EParserException.Create('Signal Processing Exception');
  end;
end;



// +++++++++++++++++++++++++++ TXmlStandardHandler +++++++++++++++++++++++++++
procedure TXmlStandardHandler.Notification(AComponent: TComponent; operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FNextHandler)
    then FNextHandler := nil;
end;

procedure TXmlStandardHandler.processSignal(const signal: TXmlSignal);
var
  Accept: Boolean;
begin
  Accept := True;
  if Assigned(FOnSignal) then
    FOnSignal(Self, Signal, Accept);
  if Accept and Assigned(NextHandler) then
    NextHandler.ProcessSignal(Signal);
  if Assigned(FOnSignaled) then
    FOnSignaled(Self, Signal);
end;



// +++++++++++++++++++++++++++++ TXmlHandlerItem +++++++++++++++++++++++++++++
function TXmlHandlerItem.getXmlHandler: TXmlCustomHandler;
begin
  Result := FXmlHandler;
end;

procedure TXmlHandlerItem.setXmlHandler(Value: TXmlCustomHandler);
begin
  FXmlHandler:= Value;
end;

procedure TXmlHandlerItem.Assign(Source: TPersistent);
begin
  if Source is TXmlHandlerItem
    then XmlHandler:= TXmlHandlerItem(Source).XmlHandler
    else inherited Assign(Source);
end;


// +++++++++++++++++++++++++++++++ TXmlHandlers ++++++++++++++++++++++++++++++
constructor TXmlHandlers.Create(Distributor: TXmlDistributor);
begin
  inherited Create(TXmlHandlerItem);
  FDistributor:= Distributor;
end;

function TXmlHandlers.GetItem(Index: Integer): TXmlHandlerItem;
begin
  Result:= TXmlHandlerItem(inherited GetItem(Index));
end;

procedure TXmlHandlers.SetItem(Index: Integer; Value: TXmlHandlerItem);
begin
  inherited SetItem(Index, Value);
end;

function TXmlHandlers.GetOwner: TPersistent;
begin
  Result:= FDistributor;
end;

function TXmlHandlers.Add: TXmlHandlerItem;
begin
  Result:= TXmlHandlerItem(inherited Add);
end;

procedure TXmlHandlers.Assign(Source: TPersistent);
var
  I : Integer;
begin
  if Source = Self then Exit;
  if Source is TStrings then begin
    Clear;
    with TStrings(Source) do
      for I := 0 to Pred(Count) do
        if Assigned(Objects[I])
          then if Objects[I] is TXmlCustomHandler
            then Self.Add.XmlHandler:= TXmlCustomHandler(Objects[I]);
  end else inherited Assign(Source);
end;

function TXmlHandlers.FindHandlerItem(AHandler: TXmlCustomHandler): TXmlHandlerItem;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
  begin
    Result := TXmlHandlerItem(inherited GetItem(I));
    if Result.FXmlHandler = AHandler then Exit;
  end;
  Result:= nil;
end;



// +++++++++++++++++++++++++++++ TXmlDistributor +++++++++++++++++++++++++++++
constructor TXmlDistributor.create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDisableCount := 0;
  FNextHandlers := TXmlHandlers.Create(Self);
end;

destructor TXmlDistributor.destroy;
begin
  FNextHandlers.Free;
  inherited Destroy;
end;

procedure TXmlDistributor.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  // Advice the Filer to read or write the NextHandlers collection as if it
  // were a property:
  Filer.DefineProperty('NextHandlers', ReadData, WriteData, True);
end;

procedure TXmlDistributor.Notification(AComponent: TComponent; operation: TOperation);
var
  HandlerItem: TXmlHandlerItem;
begin
  inherited Notification(AComponent, Operation);
  if not (csDestroying in ComponentState) and (Operation = opRemove) then begin
    if (AComponent is TXmlCustomHandler) then  begin
      HandlerItem := NextHandlers.FindHandlerItem(TXmlCustomHandler(AComponent));
      if HandlerItem <> nil then HandlerItem.XmlHandler:= nil;
    end;
  end;
end;

procedure TXmlDistributor.processSignal(const signal: TXmlSignal);
var
  I: Integer;
  Ok: Boolean;
  SignalCopy: TXmlSignal;
begin
  OK := True;
  with NextHandlers do begin
    for I := 0 to Pred(Count) do begin
      if not Assigned(Items[I].XmlHandler) then Continue;
      SignalCopy := Signal.CloneSignal(Signal.Reader);  // We use a copy of the signal,
      try                                               // because subsequent Signal Handlers
        Items[I].XmlHandler.ProcessSignal(SignalCopy);  // might change the singnal's properties.
      except
        Ok := False;
      end;
      SignalCopy.Free;
    end;
  end;
  if not Ok then
    raise EParserException.Create('Signal Processing Exception');
end;

procedure TXmlDistributor.readData(Reader: TReader);
begin
  Reader.ReadCollection(NextHandlers);
end;

procedure TXmlDistributor.setNextHandlers(const value: TXmlHandlers);
begin
  FNextHandlers.Assign(Value);
end;

procedure TXmlDistributor.writeData(Writer: TWriter);
begin
  Writer.WriteCollection(NextHandlers);
end;



// +++++++++++++++++++++++ TXmlWFTestHandler +++++++++++++++++++++++
constructor TXmlWFTestHandler.create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActivityStatus := asInactive;
  FDoctypeFound := False;
  FPrefixStack := TUtilsWideStringList.Create;
  FRootProcessingStatus := rsBeforeRoot;
  FTagStack := TUtilsWideStringList.Create;
end;

destructor TXmlWFTestHandler.Destroy;
begin
  FPrefixStack.Free;
  FTagStack.Free;
  inherited Destroy;
end;

procedure TXmlWFTestHandler.ProcessSignal(const Signal: TXmlSignal);

  procedure CheckDoctypeSignal(const DoctypeSignal: TXmlDoctypeSignal;
                                 var AXmlErrorType:  TXmlErrorType;
                                 var AFlaw: WideString);
  begin
    if FDoctypeFound then begin
      AXmlErrorType := ET_DOUBLE_DOCTYPE;
      AFlaw := DoctypeSignal.DoctypeName;
    end else begin;
      FDoctypeFound := True;
      if FRootProcessingStatus <> rsBeforeRoot then begin
        AXmlErrorType := ET_WRONG_ORDER;
        AFlaw := DoctypeSignal.DoctypeName;
      end;
    end;
  end;

  procedure CheckEndElementSignal(const EndElementSignal: TXmlEndElementSignal;
                                    var AXmlErrorType:  TXmlErrorType;
                                    var AFlaw: WideString);
  var
    LastItemIndex: Integer;
  begin
    LastItemIndex := Pred(FTagStack.Count);
    if LastItemIndex = -1 then begin
      AXmlErrorType := ET_MISSING_START_TAG;
      AFlaw := EndElementSignal.TagName;
    end else begin
      if FTagStack[LastItemIndex] = EndElementSignal.TagName then begin
        FTagStack.Delete(LastItemIndex);
        if LastItemIndex = 0 then
          FRootProcessingStatus := rsAfterRoot;
      end else begin
        AXmlErrorType := ET_MISSING_START_TAG;
        AFlaw := EndElementSignal.TagName;
      end;
    end;
  end;

  procedure CheckEndPrefixMappingSignal(const EndPrefixMappingSignal: TXmlEndPrefixMappingSignal;
                                          var AXmlErrorType:  TXmlErrorType;
                                          var AFlaw: WideString);
  var
    L: Integer;
  begin
    L := Pred(FPrefixStack.Count);
    if L = -1 then begin
      AXmlErrorType := ET_WRONG_PREFIX_MAPPING_NESTING;
      AFlaw := EndPrefixMappingSignal.Prefix;
    end else begin
      if FPrefixStack[L] <> EndPrefixMappingSignal.Prefix then begin
        AXmlErrorType := ET_WRONG_PREFIX_MAPPING_NESTING;
        AFlaw := EndPrefixMappingSignal.Prefix;
      end else
        FPrefixStack.Delete(L);
    end;
  end;

var
  Flaw: WideString;
  XmlErrorType: TXmlErrorType;
begin
  XmlErrorType := ET_None;
  Flaw := '';

  case FActivityStatus of

    asDocActive: begin

      if Signal is TXmlCDATASignal then begin
        if FRootProcessingStatus <> rsInRoot then begin
          XmlErrorType := ET_NOT_IN_ROOT;
          Flaw := TXmlCDATASignal(Signal).Data;
        end;
      end else

      if Signal is TXmlDoctypeSignal then begin
        CheckDoctypeSignal(TXmlDoctypeSignal(Signal), XmlErrorType, Flaw);
      end else

      if Signal is TXmlEndElementSignal then begin
        CheckEndElementSignal(TXmlEndElementSignal(Signal), XmlErrorType, Flaw);
      end else

      if Signal is TXmlEntityRefSignal then begin
        if FRootProcessingStatus <> rsInRoot then begin
          XmlErrorType := ET_NOT_IN_ROOT;
          Flaw := concat('&',TXmlEntityRefSignal(Signal).EntityName,';');
        end;
      end else

      if Signal is TXmlPCDATASignal then begin
        if FRootProcessingStatus <> rsInRoot then begin
          if TXmlPCDATASignal(Signal).CharRefGenerated then begin
            XmlErrorType := ET_NOT_IN_ROOT;
            Flaw := '&#';
          end else if not IsXmlS(TXmlPCDATASignal(Signal).Data) then begin
            XmlErrorType := ET_NOT_IN_ROOT;
            Flaw := TXmlPCDATASignal(Signal).Data;
          end;
        end;
      end else

      if Signal is TXmlSkippedEntitySignal then begin
        // xxx Test for wellformedness?
      end else

      if Signal is TXmlStartElementSignal then begin
        if FRootProcessingStatus = rsAfterRoot then begin
          XmlErrorType := ET_DOUBLE_ROOT_ELEMENT;
          Flaw := TXmlStartElementSignal(Signal).TagName;
        end else begin
          FRootProcessingStatus := rsInRoot;
          FTagStack.Add(TXmlStartElementSignal(Signal).TagName);
        end;
      end else

      if Signal is TXmlStartPrefixMappingSignal then begin
        FPrefixStack.Add(TXmlStartPrefixMappingSignal(Signal).Prefix);
      end else

      if Signal is TXmlEndPrefixMappingSignal then begin
        CheckEndPrefixMappingSignal(TXmlEndPrefixMappingSignal(Signal), XmlErrorType, Flaw);
      end else

      if Signal is TXmlCompletedSignal then begin
        case FRootProcessingStatus of
          rsBeforeRoot: begin
            XmlErrorType := ET_ROOT_NOT_FOUND;
            Flaw := '';
          end;
          rsInRoot: begin
            XmlErrorType := ET_MISSING_END_TAG;
            Flaw := ''; // xxx Return the name of the missing end tag?
          end
        else
          FDoctypeFound := False;
          FRootProcessingStatus := rsBeforeRoot;
        end;
        FActivityStatus := asInactive;
      end else

      if Signal is TXmlAbortedSignal then begin
        Reset;
      end else

      if not (ssDoc in Signal.Scope) then
        raise EParserException.Create('Internal Parser Exception');
    end;

    asDocFragActive: begin

      if Signal is TXmlDoctypeSignal then begin
        CheckDoctypeSignal(TXmlDoctypeSignal(Signal), XmlErrorType, Flaw);
      end else

      if Signal is TXmlEndElementSignal then begin
        CheckEndElementSignal(TXmlEndElementSignal(Signal), XmlErrorType, Flaw);
      end else

      if Signal is TXmlSkippedEntitySignal then begin
        // xxx Test for wellformedness?
      end else

      if Signal is TXmlStartElementSignal then begin
        FRootProcessingStatus := rsInRoot;
        FTagStack.Add(TXmlStartElementSignal(Signal).TagName);
      end else

      if Signal is TXmlStartPrefixMappingSignal then begin
        FPrefixStack.Add(TXmlStartPrefixMappingSignal(Signal).Prefix);
      end else

      if Signal is TXmlEndPrefixMappingSignal then begin
        CheckEndPrefixMappingSignal(TXmlEndPrefixMappingSignal(Signal), XmlErrorType, Flaw);
      end else

      if Signal is TXmlCompletedSignal then begin
        if FRootProcessingStatus = rsInRoot then begin
          XmlErrorType := ET_MISSING_END_TAG;
          Flaw := ''; // xxx Return the name of the missing end tag?
        end else begin
          FDoctypeFound := False;
          FRootProcessingStatus := rsBeforeRoot;
        end;
        FActivityStatus := asInactive;
      end else

      if Signal is TXmlAbortedSignal then begin
        Reset;
      end else

      if not (ssDoc in Signal.Scope) then
        raise EParserException.Create('Internal Parser Exception');
    end;

    asExtDtdActive, asIntDtdActive: begin

      if not (ssDtd in Signal.Scope) then
        raise EParserException.Create('Internal Parser Exception');

      if Signal is TXmlCompletedSignal then
        FActivityStatus := asInactive;

    end;

    asInactive: begin

      if (Signal is TXmlStartDocumentSignal) then begin
        FActivityStatus := asDocActive;
        FPrefixStack.Clear;
        FTagStack.Clear;
        FDoctypeFound := False;
        FRootProcessingStatus := rsBeforeRoot;
      end else

      if (Signal is TXmlStartDocumentFragmentSignal) then begin
        FActivityStatus := asDocFragActive;
        FPrefixStack.Clear;
        FTagStack.Clear;
        FDoctypeFound := False;
        FRootProcessingStatus := rsBeforeRoot;
      end else

      if Signal is TXmlStartExtDtdSignal then begin
        FActivityStatus:= asExtDtdActive;
      end else

      if Signal is TXmlStartIntDtdSignal then begin
        FActivityStatus:= asIntDtdActive;
      end else

      if Signal is TXmlAbortedSignal then begin
        Reset;
      end else

        raise EParserException.Create('Internal Parser Exception');
    end;

  end; {case ...}

  if XmlErrorType = ET_NONE then
    Signal.CheckWellformedness(XmlErrorType, Flaw);

  if XmlErrorType = ET_NONE then begin
    if Assigned(NextHandler) then
      NextHandler.ProcessSignal(Signal);
  end else
    SendErrorNotification(Signal.Reader, XmlErrorType, Signal, Flaw);
end;

procedure TXmlWFTestHandler.Notification(AComponent: TComponent; operation: TOperation);
begin
  inherited notification(AComponent,Operation);
  if (Operation = opRemove) and (AComponent = FNextHandler)
    then FNextHandler:= nil;
end;

procedure TXmlWFTestHandler.Reset;
begin
  FActivityStatus := asInactive;
  FDoctypeFound := False;
  FRootProcessingStatus := rsBeforeRoot;
  FPrefixStack.Clear;
  FTagStack.Clear;
end;



// ++++++++++++++++++++++++++++ TXmlDomBuilder ++++++++++++++++++++++++++++
constructor TXmlDomBuilder.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  FRefNode:= nil;
  FAutoPrepare:= AP_NO;
  FBuildNamespaceTree:= false;
  FKeepCDATASections:= true;
  FKeepComments:= true;
  FKeepDocumentTypeDecl:= true;
  FKeepEntityRefs:= true;
  FPrefixUriList:= TUtilsNameValueList.create;
end;

destructor TXmlDomBuilder.destroy;
begin
  FPrefixUriList.free;
  inherited destroy;
end;

procedure TXmlDomBuilder.processSignal(const signal: TXmlSignal);
var
  i,j: integer;
  prfx,localName,AttrNsUri: wideString;
  elementNsUri: wideString; // = ''
  newCData: TdomCDATASection;
  newComment: TdomComment;
  newDocType: TdomDocumentTypeDecl;
  newElement: TdomElement;
  newEntityRef: TdomEntityReference;
  newPI: TdomProcessingInstruction;
begin
  if signal is TXmlCDATASignal then begin
    if assigned(FRefNode) then begin
      if FKeepCDATASections then begin
        newCData := FRefNode.referenceDocument.CreateCDATASection(TXmlCDATASignal(signal).Data);
        try
          FRefNode.appendChild(newCData);
        except
          newCData.Free;
          raise;
        end; {try ...}
      end else
        writePCDATA(signal.Reader,signal,TXmlCDATASignal(signal).Data);
    end; {if assigned(FRefNode) ...}
  end else


  if signal is TXmlCommentSignal then begin
    if FKeepComments then begin
      if assigned(FRefNode) then begin
        newComment := FRefNode.referenceDocument.CreateComment(TXmlCommentSignal(signal).Data);
        try
          FRefNode.appendChild(newComment);
        except
          newComment.Free;
          raise;
        end; {try ...}
      end; {if assigned(FRefNode) ...}
    end; {if FKeepComments ...}
  end else


  if signal is TXmlDoctypeSignal then begin
    if FKeepDocumentTypeDecl then begin
      if assigned(FRefNode) then begin
        newDocType := FRefNode.referenceDocument.CreateDocumentTypeDecl(
                       TXmlDoctypeSignal(signal).DoctypeName,
                       TXmlDoctypeSignal(signal).PublicId,
                       TXmlDoctypeSignal(signal).SystemId,
                       TXmlDoctypeSignal(signal).Data);
        try
          FRefNode.appendChild(newDocType);
          case AutoPrepare of
            AP_INTERNAL_DECLARATIONS:
              if not newDocType.Prepare(True,
                                        TXmlDoctypeSignal(signal).IntSubsetStartByteNumber,
                                        TXmlDoctypeSignal(signal).IntSubsetStartCharNumber,
                                        TXmlDoctypeSignal(signal).IntSubsetStartColumn,
                                        TXmlDoctypeSignal(signal).IntSubsetStartLine) then
                raise EParserException.Create('Non-wellformed DTD.');
            AP_COMPLETE:
              if not newDocType.Prepare(False,
                                        TXmlDoctypeSignal(signal).IntSubsetStartByteNumber,
                                        TXmlDoctypeSignal(signal).IntSubsetStartCharNumber,
                                        TXmlDoctypeSignal(signal).IntSubsetStartColumn,
                                        TXmlDoctypeSignal(signal).IntSubsetStartLine) then
                raise EParserException.Create('Non-wellformed DTD.');
          end;
        except
          newDocType.Free;
          raise;
        end; {try ...}
      end; {if assigned(FRefNode) ...}
    end; {if FKeepDocumentTypeDecl ...}
  end else


  if signal is TXmlEndElementSignal then begin
    if assigned(FRefNode) then
      FRefNode:= FRefNode.ParentNode;
  end else


  if signal is TXmlEndPrefixMappingSignal then begin
    with FPrefixUriList do
      delete(pred(length));
  end else


  if signal is TXmlEntityRefSignal then begin
    if assigned(FRefNode) then begin
      if KeepEntityRefs or not isXmlPredefinedEntityName(TXmlEntityRefSignal(signal).EntityName) then begin
        newEntityRef:= FRefNode.referenceDocument.CreateEntityReference(TXmlEntityRefSignal(signal).EntityName);
        try
          FRefNode.appendChild(newEntityRef);
        except
          newEntityRef.Free;
          raise;
        end; {try ...}
      end else begin
        if TXmlEntityRefSignal(signal).EntityName = 'lt' then begin
          writePCDATA(signal.Reader,signal,#60);
        end else if TXmlEntityRefSignal(signal).EntityName = 'gt' then begin
          writePCDATA(signal.Reader,signal,#62);
        end else if TXmlEntityRefSignal(signal).EntityName = 'amp' then begin
          writePCDATA(signal.Reader,signal,#38);
        end else if TXmlEntityRefSignal(signal).EntityName = 'apos' then begin
          writePCDATA(signal.Reader,signal,#39);
        end else if TXmlEntityRefSignal(signal).EntityName = 'quot' then begin
          writePCDATA(signal.Reader,signal,#34);
        end;
      end; {if ... else}
    end; {if assigned(FRefNode) ...}
  end else


  if signal is TXmlPCDATASignal then begin
    if assigned(FRefNode) then
      if FRefNode.NodeType <> ntDocument_Node then
        writePCDATA(signal.Reader,signal,TXmlPCDATASignal(signal).Data);
  end else


  if signal is TXmlProcessingInstructionSignal then begin
    if assigned(FRefNode) then begin
      newPI:= FRefNode.referenceDocument.CreateProcessingInstruction(TXmlProcessingInstructionSignal(signal).Target,TXmlProcessingInstructionSignal(signal).Data);
      try
        FRefNode.appendChild(newPI);
      except
        newPI.Free;
        raise;
      end;
    end; {if assigned(FRefNode) ...}
  end else


  if signal is TXmlSkippedEntitySignal then begin
    // notifications through skippedEntity() are being ignored.
  end else


  if signal is TXmlStartDocumentSignal then begin
    FPrefixUriList.clear;

    if assigned(FRefNode) then begin
      if (FRefNode.nodeType = ntDocument_Node) then begin
        with (FRefNode as TdomDocument) do begin
          inputEncoding:= TXmlStartDocumentSignal(signal).EncodingName;
          xmlEncoding:= TXmlStartDocumentSignal(signal).EncodingName;
          xmlStandalone:= TXmlStartDocumentSignal(signal).StandaloneDecl;
          xmlVersion:= TXmlStartDocumentSignal(signal).Version;
          documentUri:= signal.uri
        end;
      end;
    end;
  end else


  if signal is TXmlStartDocumentFragmentSignal then begin
    FPrefixUriList.clear;
  end else


  if signal is TXmlStartElementSignal then begin
    if assigned(FRefNode) then begin

      if BuildNamespaceTree then begin

        // Parse into namespace-aware document tree:

        xmlExtractPrefixAndLocalName(TXmlStartElementSignal(signal).TagName,prfx,localName);
        with FPrefixUriList do begin
          i:= indexOfName(prfx);
          if i > -1 then
            elementNsUri:= values[i];
        end; {with ...}

        newElement:= FRefNode.referenceDocument.CreateElementNS(elementNsUri,TXmlStartElementSignal(signal).TagName);
        FRefNode.appendChild(newElement);
        FRefNode:= newElement;

        // Compute attributes:

        for i:= 0 to pred(TXmlStartElementSignal(signal).Attributes.Length) do
          if TXmlStartElementSignal(signal).Attributes.names[i] = 'xmlns' then begin
            newElement.SetAttributeNS('http://www.w3.org/2000/xmlns/','xmlns',TXmlStartElementSignal(signal).Attributes.values[i]);
          end else begin
            xmlExtractPrefixAndLocalName(TXmlStartElementSignal(signal).Attributes.names[i],prfx,localName);
            if prfx = '' then begin
              attrNsUri:= '';
            end else if prfx = 'xml' then begin
              attrNsUri:= 'http://www.w3.org/XML/1998/namespace';
            end else if prfx = 'xmlns' then begin
              attrNsUri:= 'http://www.w3.org/2000/xmlns/';
            end else begin
              j:= FPrefixUriList.indexOfName(prfx);
              if j > -1 then
                attrNsUri:= FPrefixUriList.values[j];
            end;
            newElement.SetAttributeNS(attrNsUri,TXmlStartElementSignal(signal).Attributes.names[i],TXmlStartElementSignal(signal).Attributes.values[i])
          end; {if attributes.names[i] = 'xmlns' ... else ...}

      end else begin {if BuildNamespaceTree ...}

        // Parse into non-namespace-aware document tree:

        newElement:= FRefNode.referenceDocument.CreateElement(TXmlStartElementSignal(signal).TagName);
        FRefNode.appendChild(newElement);
        FRefNode:= newElement;

        // Compute attributes:
        for i:= 0 to pred(TXmlStartElementSignal(signal).Attributes.Length) do
          newElement.SetAttribute(TXmlStartElementSignal(signal).Attributes.Names[i],
                                  TXmlStartElementSignal(signal).Attributes.Values[i]);

      end; {if BuildNamespaceTree ... else ...}

    end; {if assigned(FRefNode) ...}
  end else


  if signal is TXmlStartPrefixMappingSignal then begin
    FPrefixUriList.add(TXmlStartPrefixMappingSignal(signal).Prefix,
                       TXmlStartPrefixMappingSignal(signal).Uri);
  end else


  if Signal is TXmlAbortedSignal then begin
    Reset;
  end else

  if not (ssDoc in signal.Scope) then
    raise EParserException.create('Internal Parser Exception');

end;

procedure TXmlDomBuilder.Reset;
begin
  FPrefixUriList.clear;
end;

procedure TXmlDomBuilder.WritePCDATA(const sender: TXmlCustomReader;
                                     const locator: IDomLocator;
                                     const data: wideString);
var
  newPcdata: TdomText;
begin
  if assigned(FRefNode.LastChild) and (FRefNode.LastChild.NodeType = ntText_Node)
    then (FRefNode.LastChild as TdomText).appendData(data)
    else begin
      newPcdata:= FRefNode.referenceDocument.CreateTextNode(data);
      try
        FRefNode.appendChild(newPcdata);
      except
        newPcdata.Free;
        raise;
      end; {try ...}
    end;
end;



// ++++++++++++++++++++++++++ TXmlASBuilder +++++++++++++++++++++++++++
constructor TXmlASBuilder.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  FASModel:= nil;
end;

procedure TXmlASBuilder.insertMixedContent(const sender: TXmlCustomReader;
                                           const refASElementDecl: TdomASElementDecl;
                                           const contSpec: wideString);
var
  dummy, content,piece: wideString;
  freq: TdomASFrequency;
  separator: integer;
  Error: boolean;
  newASContentModel: TdomASContentModel;
begin
  // xxx Routine can perhaps be optimized.

  content:= trimWhitespace(contSpec);
  freq:= AS_REQUIRED_FRQ;
  if (content[length(content)] = '*') then begin
    freq:= AS_ZERO_OR_MORE_FRQ;
    dummy:= copy(content,1,length(content)-1);
    content:= dummy;
  end;
  if length(content) = 0
    then raise EParserException.create('Parser error.');
  if wideChar(content[length(content)]) <> ')'
    then raise EParserException.create('Parser error.');
  XMLTruncRoundBrackets(content,dummy,Error);
  if Error or (dummy = '')
    then raise EParserException.create('Parser error.');
  content:= dummy;
  newASContentModel:= refASElementDecl.createContentModel('',AS_CHOICE_CM);
  newASContentModel.frequency:= freq;
  refASElementDecl.replaceContentModel(newASContentModel);
  if content = '#PCDATA' then begin
    if (freq <> AS_REQUIRED_FRQ) and (freq <> AS_ZERO_OR_MORE_FRQ)
      then raise EParserException.create('Parser error.');
    exit;
  end;
  if freq <> AS_ZERO_OR_MORE_FRQ
    then raise EParserException.create('Parser error.');
  separator:= pos(wideString('|'),content);
  if separator = 0 then raise EParserException.create('Parser error.');
  dummy:= trimWhitespace(copy(content,separator+1,length(content)-separator));
  content:= dummy;
  while content <> '' do begin
    separator:= pos(wideString('|'),content);
    if separator = 0 then begin
      piece:= content;
      content:= '';
    end else begin
      piece:= trimWhitespace(copy(content,1,separator-1));
      dummy:= trimWhitespace(copy(content,separator+1,length(content)-separator));
      content:= dummy;
      if content = '' then raise EParserException.create('Parser error.');
    end; {if ...}
    if not IsXmlName(piece) then raise EParserException.create('Parser error.');
    newASContentModel.subModels.appendASNode(refASElementDecl.createContentModel(piece,AS_ELEMENT_CM));
  end; {while ...}
end;

procedure TXmlASBuilder.insertChildrenContent(const sender: TXmlCustomReader;
                                              const refASObject: TdomASObject;
                                              const contSpec: wideString);

var
  piece,dummy,content: wideString;
  SeparatorChar: WideChar;
  freq: TdomASFrequency;
  j,i,bracketNr: integer;
  newASContentModel_1,newASContentModel_2: TdomASContentModel;
  Error: boolean;
begin
  content:= trimWhitespace(contSpec);
  if content[length(content)] = WideChar('?') then begin
    freq:= AS_OPTIONAL_FRQ;
    dummy:= copy(content,1,length(content)-1);
    content:= dummy;
  end else if content[length(content)] = WideChar('*') then begin
    freq:= AS_ZERO_OR_MORE_FRQ;
    dummy:= copy(content,1,length(content)-1);
    content:= dummy;
  end else if content[length(content)] = WideChar('+') then begin
    freq:= AS_ONE_OR_MORE_FRQ;
    dummy:= copy(content,1,length(content)-1);
    content:= dummy;
  end else freq:= AS_REQUIRED_FRQ;
  if length(content) = 0
    then raise EParserException.create('Parser error.');
  if wideChar(content[length(content)]) <> ')'
    then raise EParserException.create('Parser error.');
  XMLTruncRoundBrackets(content,dummy,Error);
  if Error or (dummy = '')
    then raise EParserException.create('Parser error.');
  content:= dummy;

  bracketNr:= 0;
  SeparatorChar:= ',';
  for i:= 1 to length(content) do begin
    if (content[i] = ',') and (bracketNr = 0) then begin
      SeparatorChar:= ',';
      break;
    end; {if ...}
    if (content[i] = '|') and (bracketNr = 0) then begin
      SeparatorChar:= '|';
      break;
    end; {if ...}
    if content[i] = '(' then inc(bracketNr);
    if content[i] = ')' then begin
      if bracketNr = 0 then raise EParserException.create('Parser error.');
      dec(bracketNr);
    end;
  end; {for ...}

  if SeparatorChar = ',' then begin
    case refASObject.objectType of
      AS_CONTENT_MODEL:
      begin
        newASContentModel_1:= (refASObject as TdomASContentModel).ownerElementDecl.createContentModel('',AS_SEQUENCE_CM);
        newASContentModel_1.frequency:= freq;
        (refASObject as TdomASContentModel).subModels.appendASNode(newASContentModel_1);
      end;
      AS_ELEMENT_DECLARATION:
      begin
        newASContentModel_1:= (refASObject as TdomASElementDecl).createContentModel('',AS_SEQUENCE_CM);
        newASContentModel_1.frequency:= freq;
        (refASObject as TdomASElementDecl).replaceContentModel(newASContentModel_1);
      end;
    else
      raise EParserException.create('Parser error.');
    end;
  end else begin
    case refASObject.objectType of
      AS_CONTENT_MODEL:
      begin
        newASContentModel_1:= (refASObject as TdomASContentModel).ownerElementDecl.createContentModel('',AS_CHOICE_CM);
        newASContentModel_1.frequency:= freq;
        (refASObject as TdomASContentModel).subModels.appendASNode(newASContentModel_1);
      end;
      AS_ELEMENT_DECLARATION:
      begin
        newASContentModel_1:= (refASObject as TdomASElementDecl).createContentModel('',AS_CHOICE_CM);
        newASContentModel_1.frequency:= freq;
        (refASObject as TdomASElementDecl).replaceContentModel(newASContentModel_1);
      end;
    else
      raise EParserException.create('Parser error.');
    end;
  end;

  bracketNr:= 0;
  i:= 0;
  j:= 1;
  while i < length(content) do begin
    inc(i);
    if content[i] = '(' then inc(bracketNr);
    if content[i] = ')' then begin
      if bracketNr = 0 then raise EParserException.create('Parser error.');
      dec(bracketNr);
    end;
    if ((content[i] = SeparatorChar) and (bracketNr = 0)) or
       (i = length(content)) then begin
      if bracketNr > 0 then raise EParserException.create('Parser error.');
      if i = length(content)
        then piece:= trimWhitespace(copy(content,j,i+1-j))
        else piece:= trimWhitespace(copy(content,j,i-j));
      j:= i+1;

      if piece[1] = '(' then begin
        insertChildrenContent(sender,newASContentModel_1,piece);
      end else begin
        if piece[length(piece)] = wideChar('?') then begin
          freq:= AS_OPTIONAL_FRQ;
          dummy:= copy(piece,1,length(piece)-1);
          piece:= dummy;
        end else if piece[length(piece)] = wideChar('*') then begin
          freq:= AS_ZERO_OR_MORE_FRQ;
          dummy:= copy(piece,1,length(piece)-1);
          piece:= dummy;
        end else if piece[length(piece)] = wideChar('+') then begin
          freq:= AS_ONE_OR_MORE_FRQ;
          dummy:= copy(piece,1,length(piece)-1);
          piece:= dummy;
        end else freq:= AS_REQUIRED_FRQ;
        if not IsXmlName(piece)
          then raise EParserException.create('Parser error.');
        newASContentModel_2:= newASContentModel_1.ownerElementDecl.createContentModel(piece,AS_ELEMENT_CM);
        newASContentModel_2.frequency:= freq;
        newASContentModel_1.subModels.appendASNode(newASContentModel_2);
      end; {if ...}

    end; {if ...}
  end; {while ...}

end;

procedure TXmlASBuilder.setASModel(const value: TdomASModel);
begin
  FASModel := value;
end;

procedure TXmlASBuilder.processSignal(const signal: TXmlSignal);

var
  AttListElementDecl: TdomASElementDecl;
  ContSpec: WideString;
  ContspecType: TdomASContentType;
  NewASAttributeDecl: TdomASAttributeDecl;
  NewElementDecl: TdomASElementDecl;
  NewEntityDecl: TdomASEntityDecl;
  NewNotationDecl: TdomASNotationDecl;
begin
  if not Assigned(FASModel) then
    Exit;

  if Signal is TXmlAttributeDefinitionSignal then begin
    with TXmlAttributeDefinitionSignal(Signal) do begin
      FASModel.SetASElementDecl(ElementName, AS_UNKNOWN_CONTENTTYPE, AttListElementDecl);
      if not AttListElementDecl.SetASAttributeDecl(AttributeName, DefaultValue,
        Enumeration, AttributeType, Constraint, NewASAttributeDecl) then
          SendErrorNotification(Signal.Reader, ET_DOUBLE_ATTDEF, Signal,
          TXmlAttributeDefinitionSignal(Signal).AttributeName);
    end;
  end else

  if Signal is TXmlElementTypeDeclarationSignal then begin
    ContSpec := TrimWhitespace(TXmlElementTypeDeclarationSignal(Signal).Data);
    ContspecType := AS_ELEMENT_CONTENTTYPE;
    if ContSpec = 'EMPTY'
      then ContspecType := AS_EMPTY_CONTENTTYPE
      else if ContSpec = 'ANY'
        then ContspecType := AS_ANY_CONTENTTYPE
        else if Pos('#PCDATA', ContSpec) > 0
          then ContspecType := AS_MIXED_CONTENTTYPE;
    try
      if FASModel.SetASElementDecl(TXmlElementTypeDeclarationSignal(Signal).ElementName,
        ContspecType, NewElementDecl)
      then begin
        case contspecType of
          AS_MIXED_CONTENTTYPE: InsertMixedContent(Signal.Reader, NewElementDecl, ContSpec);
          AS_ELEMENT_CONTENTTYPE: InsertChildrenContent(Signal.Reader, NewElementDecl, ContSpec);
        end;
      end;  // Remark: Silently skip double Element Type Declarations.
            // Violations of VC: Unique Element Type Declaration
            // (XML 1.0, § 3.2) are checked in
            // TXmlStandardDtdReader.WriteElementDeclaration.
    except
      SendErrorNotification(Signal.Reader, ET_INVALID_ELEMENT_DECL, Signal,
        TXmlElementTypeDeclarationSignal(Signal).ElementName);
    end; {try ...}
  end else

  if Signal is TXmlEntityDeclarationSignal then begin
    if FASModel.SetASEntityDecl(TXmlEntityDeclarationSignal(Signal).EntityName,
                                ResolveCharRefs(TXmlEntityDeclarationSignal(Signal).EntityValue),
                                TXmlEntityDeclarationSignal(Signal).PublicId,
                                TXmlEntityDeclarationSignal(Signal).SystemId,
                                TXmlEntityDeclarationSignal(Signal).NotationName,
                                NewEntityDecl) then begin
      if NewEntityDecl.Usability = AS_UNUSABLE then
        SendErrorNotification(Signal.Reader, ET_UNUSABLE_ENTITY_DECL, Signal,
          TXmlEntityDeclarationSignal(Signal).EntityName);
    end else
      SendErrorNotification(Signal.Reader, ET_DOUBLE_ENTITY_DECL, Signal,
        TXmlEntityDeclarationSignal(Signal).EntityName);
  end else

  if Signal is TXmlNotationDeclarationSignal then begin
    if not FASModel.SetASNotationDecl(TXmlNotationDeclarationSignal(Signal).NotationName,
                                      TXmlNotationDeclarationSignal(Signal).PublicId,
                                      TXmlNotationDeclarationSignal(Signal).SystemId,
                                      newNotationDecl) then
      SendErrorNotification(Signal.Reader, ET_DUPLICATE_NOTATION_DECL, Signal,
        TXmlNotationDeclarationSignal(Signal).NotationName);
  end else

  if Signal is TXmlStartExtDtdSignal then begin
    FASModel.Location := TXmlStartExtDtdSignal(Signal).SystemId;
  end else

  if Signal is TXmlStartIntDtdSignal then begin
    FASModel.Location := TXmlStartIntDtdSignal(Signal).SystemId;
  end else

  if not (ssDtd in Signal.Scope) then
    raise EParserException.Create('Internal Parser Exception');
end;



// ++++++++++++++++++++++++ TXmlStreamBuilder ++++++++++++++++++++++++++
constructor TXmlStreamBuilder.create(aOwner: TComponent);
begin
  inherited;
  FIncludeXmlDecl := True;
  FCurrentEncoding:= '';
  FDefaultEncoding:= '';
  FDefaultCodecClass := nil;
  resetCurrentCodecClass;
  FUseByteOrderMark:= false;
  FOutputSource:= nil;
  FOpenElementsCount:= 0;
  FAttListDeclIsOpen := False;
  FByteCount:= 0;
  FCharacterCount:= 0;
  FColumnCount:= 0;
  FLineFeedCount:= 0;
  FTabCount:= 0;
end;

procedure TXmlStreamBuilder.doAfterWrite(const pieceType: TdomPieceType;
                                         const Locator:IDomLocator);
begin
  if assigned(FOnAfterWrite) then
    FOnAfterWrite(Self, PieceType, Locator);
end;

procedure TXmlStreamBuilder.doBeforeWrite(const pieceType: TdomPieceType;
                                          const Locator: IDomLocator);
begin
  if assigned(FOnBeforeWrite) then
    FOnBeforeWrite(Self, PieceType, Locator);
end;

procedure TXmlStreamBuilder.CheckAttListDeclarationClosed(const Sender: TXmlCustomReader;
                                                          const Locator: IDomLocator);
begin
  if FAttListDeclIsOpen then begin
    WriteWideStrings(Sender, Locator, ['>'], False);
    FAttListDeclIsOpen := False;
    DoAfterWrite(xmlAttributeDecl, Locator);
  end;
end;

procedure TXmlStreamBuilder.CheckAttListDeclarationOpen(const Sender: TXmlCustomReader;
                                                        const Locator: IDomLocator;
                                                        const elementName: WideString);
begin
  if FAttListDeclIsOpen then begin
    if FCurrentAttListDeclName <> elementName then begin
      WriteWideStrings(Sender,Locator,['>'], False);
      DoAfterWrite(xmlAttributeDecl, Locator);
      DoBeforeWrite(xmlAttributeDecl, Locator);
      WriteWideStrings(Sender,Locator,[#10'<!ATTLIST ', elementName, #10], False);
      FCurrentAttListDeclName := elementName;
    end;
  end else begin
    DoBeforeWrite(xmlAttributeDecl, Locator);
    WriteWideStrings(Sender,Locator,[#10'<!ATTLIST ', elementName, #10], False);
    FCurrentAttListDeclName := elementName;
    FAttListDeclIsOpen := True;
  end;
end;

function TXmlStreamBuilder.getCurrentCodecClass: TUnicodeCodecClass;
begin
  if assigned(FOutputSource)
    then Result:= FOutputSource.codecClass
    else Result:= nil;
end;

procedure TXmlStreamBuilder.putCurrentCodecClass(const value: TUnicodeCodecClass);
begin
  if assigned(FOutputSource) then begin
    if assigned(value)
      then FOutputSource.codecClass := value
      else FOutputSource.codecClass := TUTF8Codec;
  end;
end;

procedure TXmlStreamBuilder.ResetCurrentCodecClass;
begin
  PutCurrentCodecClass(defaultCodecClass);
end;

procedure TXmlStreamBuilder.setDefaultEncoding(const value: WideString);
var
  newCodecClass: TUnicodeCodecClass;
begin
  if value = '' then begin
    FDefaultEncoding:= '';
    FDefaultCodecClass:= nil;
  end else begin
    newCodecClass:= StrToEncoding(value);
    if assigned(newCodecClass) then begin
      FDefaultCodecClass:= newCodecClass;
      FDefaultEncoding:= value;
    end else
      raise ENot_Supported_Err.create('Encoding not supported error.');
  end;
  resetCurrentCodecClass;
end;

procedure TXmlStreamBuilder.SetIncludeXmlDecl(const Value: boolean);
begin
  FIncludeXmlDecl := Value;
end;

procedure TXmlStreamBuilder.setOutputSource(const value: TXmlOutputSource);
begin
  FOutputSource:= value;
  resetCurrentCodecClass;
end;

procedure TXmlStreamBuilder.setUseByteOrderMark(const value: Boolean);
begin
  FUseByteOrderMark:= value;
end;

procedure TXmlStreamBuilder.writeByteOrderMark(const Sender: TXmlCustomReader;
                                               const Locator: IDomLocator;
                                                 out byteCount: Integer);
const
  UTF_8_BOM    : Array[0..2] of Byte = ($EF, $BB, $BF);
  UTF_16BE_BOM : Array[0..1] of Byte = ($FE, $FF);
  UTF_16LE_BOM : Array[0..1] of Byte = ($FF, $FE);
begin
  try
    if (currentCodecClass = TUTF16BECodec) or (currentCodecClass = TUCS2Codec) then begin
      byteCount := 2;
      FOutputSource.write(UTF_16BE_BOM, 2);
    end else if currentCodecClass = TUTF16LECodec then begin
      byteCount := 2;
      FOutputSource.write(UTF_16LE_BOM, 2);
    end else if currentCodecClass = TUTF8Codec then begin
        if UseByteOrderMark and assigned(FOutputSource) then begin
          byteCount := 3;
          FOutputSource.write(UTF_8_BOM, 3);
        end else byteCount := 0;
    end else byteCount := 0;
  except
    raise EParserException.Create('Signal Processing Exception');
  end;
end;

procedure TXmlStreamBuilder.writeWideString(const S: WideString;
                                            const useCharRefs: Boolean);
const
  ERROR_STR: string = 'Invalid Character';
  LF  = $A; // Line Feed
  TAB = $9; // Horizontal Tabulation
var
  CharRef: WideString;
  HighSurrogate, LowSurrogate: WideChar;
  I, J: integer;
  BytesUsed, BytesUsed_2: Integer;
  UCS4: UCS4Char;
begin
  try
    I := 1;
    while I <= Length(S) do begin
      UCS4 := Ord(S[I]);

      // Test for UTF-16 surrogates and recalculate UCS-4 codepoint if necessary:
      case UCS4 of
      $D800..$DBFF: // High surrogate of Unicode character [$10000..$10FFFF]
        begin
          if I = length(S) // End of WideString --> No low surrogate found
            then raise EWriteError.Create(ERROR_STR);
          HighSurrogate:= S[I];
          Inc(I);
          LowSurrogate:= S[I];
          if not IsUtf16LowSurrogate(LowSurrogate)  // No low surrogate found
            then raise EWriteError.Create(ERROR_STR);

          UCS4:= UTF16SurrogateToInt(HighSurrogate, LowSurrogate);
        end;
      $DC00..$DFFF: // Low surrogate, but no preceeding high surrogate
        raise EWriteError.Create(ERROR_STR);
      end; {case ...}

      try
        FOutputSource.writeUCS4Char(UCS4, BytesUsed);
      except
        on EConvertError do
          if UseCharRefs then begin
            CharRef := XmlIntToCharRefHex(UCS4);
            BytesUsed := 0;
            for J := 1 to Length(CharRef) do begin
              FOutputSource.writeUCS4Char(Ord(CharRef[J]), BytesUsed_2);
              BytesUsed := BytesUsed + BytesUsed_2;
            end;
          end else
            raise;
      end;

      // Update position properties:
      case UCS4 of
        LF: begin
          Inc(FLineFeedCount);
          FColumnCount := 0;
          FTabCount := 0;
        end;
        TAB: begin
          Inc(FTabCount);
          Inc(FColumnCount);
        end
      else
        Inc(FColumnCount);
      end;
      FByteCount := FByteCount + BytesUsed;
      Inc(FCharacterCount);

      Inc(I);
    end; {while ...}

  except
    on EConvertError do raise EWriteError.Create(ERROR_STR);
  end;
end;

procedure TXmlStreamBuilder.WriteWideStrings(const Sender: TXmlCustomReader;
                                             const Locator: IDomLocator;
                                             const xmlStrgs: array of WideString;
                                             const useCharRefs: Boolean);
var
  I: Longint;
begin
  if not Assigned(FOutputSource) then Exit;
  for I := 0 to High(XmlStrgs) do begin
    try
      WriteWideString(XmlStrgs[I], UseCharRefs);
    except
      SendErrorNotification(Sender, ET_INVALID_CHARACTER, Locator, XmlStrgs[I]);  // xxx Adapt Locator?
    end;
  end;
end;

procedure TXmlStreamBuilder.processSignal(const Signal: TXmlSignal);
begin
  if Signal is TXmlAttributeDefinitionSignal then begin
    WriteAttributeDefinitionSignal(TXmlAttributeDefinitionSignal(Signal));
  end else begin

    CheckAttListDeclarationClosed(Signal.Reader, Signal);

    if Signal is TXmlCDataSignal then begin
      WriteCDataSignal(TXmlCDataSignal(Signal));

    end else if Signal is TXmlCommentSignal then begin
      WriteCommentSignal(TXmlCommentSignal(Signal));

    end else if Signal is TXmlDoctypeSignal then begin
      WriteDoctypeSignal(TXmlDoctypeSignal(Signal));

    end else if Signal is TXmlElementTypeDeclarationSignal then begin
      WriteElementTypeDeclarationSignal(TXmlElementTypeDeclarationSignal(Signal));

    end else if Signal is TXmlEndElementSignal then begin
      WriteEndElementSignal(TXmlEndElementSignal(Signal));

    end else if Signal is TXmlEndPrefixMappingSignal then begin
      // do nothing;

    end else if Signal is TXmlEntityDeclarationSignal then begin
      WriteEntityDeclarationSignal(TXmlEntityDeclarationSignal(Signal));

    end else if Signal is TXmlEntityRefSignal then begin
      WriteEntityRefSignal(TXmlEntityRefSignal(Signal));

    end else if Signal is TXmlNotationDeclarationSignal then begin
      WriteNotationDeclarationSignal(TXmlNotationDeclarationSignal(Signal));

    end else if Signal is TXmlParameterEntityDeclarationSignal then begin
      WriteParameterEntityDeclarationSignal(TXmlParameterEntityDeclarationSignal(Signal));

    end else if Signal is TXmlPCDATASignal then begin
      WritePCDATASignal(TXmlPCDATASignal(Signal));

    end else if Signal is TXmlProcessingInstructionSignal then begin
      WriteProcessingInstructionSignal(TXmlProcessingInstructionSignal(Signal));

    end else if Signal is TXmlSkippedEntitySignal then begin
      WriteSkippedEntitySignal(TXmlSkippedEntitySignal(Signal));

    end else if Signal is TXmlStartDocumentSignal then begin
      WriteStartDocumentSignal(TXmlStartDocumentSignal(Signal));

    end else if Signal is TXmlStartDocumentFragmentSignal then begin
      WriteStartDocumentFragmentSignal(TXmlStartDocumentFragmentSignal(Signal));

    end else if Signal is TXmlStartElementSignal then begin
      WriteStartElementSignal(TXmlStartElementSignal(Signal));

    end else if Signal is TXmlStartExtDtdSignal then begin
      WriteStartExtDtdSignal(TXmlStartExtDtdSignal(Signal));

    end else if Signal is TXmlStartIntDtdSignal then begin
      WriteStartIntDtdSignal(TXmlStartIntDtdSignal(Signal));

    end else if Signal is TXmlStartPrefixMappingSignal then begin
      // do nothing;

    end else if Signal is TXmlCompletedSignal then begin
      WriteCompletedSignal(TXmlCompletedSignal(Signal));

    end else if Signal is TXmlAbortedSignal then begin
      Reset;

    end else
      raise EParserException.create('Internal Parser Exception');

  end;
end;

procedure TXmlStreamBuilder.WriteCDATASignal(const Signal: TXmlCDataSignal);
begin
  DoBeforeWrite(xmlCDATA, Signal);
  with Signal do begin
    WriteWideStrings(Reader, Signal, ['<![CDATA[', Data, ']]>'], False);
  end;
  DoAfterWrite(xmlCDATA, Signal);
end;

procedure TXmlStreamBuilder.WriteCommentSignal(const Signal: TXmlCommentSignal);
begin
  DoBeforeWrite(xmlComment, Signal);
  with Signal do begin
    if FOpenElementsCount > 0
      then WriteWideStrings(Reader, Signal, ['<!--', Data, '-->'], False)
      else WriteWideStrings(Reader, Signal, ['<!--', Data, '-->'#10], False);
  end;
  DoAfterWrite(xmlComment, Signal);
end;

procedure TXmlStreamBuilder.WriteDoctypeSignal(const Signal: TXmlDoctypeSignal);
const
  SQ: WideString = #39; // code of '
  DQ: WideString = #34; // code of "
var
  Qm: WideString;
begin                                   
  DoBeforeWrite(xmlDoctype, Signal);
  with Signal do begin
    WriteWideStrings(Reader, Signal, ['<!DOCTYPE ', DoctypeName], False);
    if SystemId = '' then begin
      if PublicId <> '' then
        WriteWideStrings(Reader, Signal, [WideString(' PUBLIC "'), PublicId, WideString('"')], False);
    end else begin
      if pos(DQ, SystemId) = 0
        then Qm := DQ
        else Qm := SQ;
      if PublicId = ''
        then WriteWideStrings(Reader, Signal, [WideString(' SYSTEM '), Qm, SystemId, Qm], False)
        else WriteWideStrings(Reader, Signal, [WideString(' PUBLIC "'), PublicId, WideString('" '), qm, SystemId, qm], False);
    end;
    if Length(data) = 0
      then WriteWideStrings(Reader, Signal, [' >'#10], False)
      else WriteWideStrings(Reader, Signal, [' [',Data,'] >'#10], False);
  end;
  DoAfterWrite(xmlDoctype, Signal);
end;

procedure TXmlStreamBuilder.WriteEndElementSignal(const Signal: TXmlEndElementSignal);
begin
  DoBeforeWrite(xmlEndTag, Signal);
  with Signal do begin
    WriteWideStrings(Reader, Signal, ['</', TagName, '>'], False);
    Dec(FOpenElementsCount);
    if FOpenElementsCount = 0 then
      WriteWideStrings(Reader, Signal, [#10], False);
  end;
  DoAfterWrite(xmlEndTag, Signal);
end;

procedure TXmlStreamBuilder.WriteEntityRefSignal(const Signal: TXmlEntityRefSignal);
begin
  DoBeforeWrite(xmlEntityRef, Signal);
  with Signal do begin
    WriteWideStrings(Reader, Signal,['&', EntityName, ';'], False);
  end;
  DoAfterWrite(xmlEntityRef, Signal);
end;

procedure TXmlStreamBuilder.WriteCompletedSignal(const Signal: TXmlCompletedSignal);
begin
  ResetCurrentCodecClass;
  FAttListDeclIsOpen := False;
  FOpenElementsCount := 0;
end;

procedure TXmlStreamBuilder.WritePCDATASignal(const Signal: TXmlPCDATASignal);
var
  I: Integer;
  Content: TUtilsCustomWideStr;
  S: WideString;
begin
  DoBeforeWrite(xmlPCDATA, Signal);
  with Signal do begin
    Content:= TUtilsCustomWideStr.Create;
    try
      for I := 1 to Length(Data) do begin
        case Ord(Data[I]) of
          38: Content.AddWideString('&amp;'); // Ampersand ('&')
          60: Content.AddWideString('&lt;');  // Less than ('<')
          62: Content.AddWideString('&gt;');  // Greater than ('>')
          13: Content.AddWideString('&#xD;'); // Carriage Return (CR)
        else
          Content.AddWideChar(Data[I]);
        end;
      end;
      S := Content.Value;
    finally
      Content.Free;
    end;

    WriteWideStrings(Reader, Signal, [S], True);
  end;
  DoAfterWrite(xmlPCDATA, Signal);
end;

procedure TXmlStreamBuilder.WriteProcessingInstructionSignal(const Signal: TXmlProcessingInstructionSignal);
begin
  DoBeforeWrite(xmlProcessingInstruction, Signal);
  with Signal do begin
    if data = '' then begin
      if FOpenElementsCount > 0
        then WriteWideStrings(Reader, Signal, ['<?', Target, '?>'], False)
        else WriteWideStrings(Reader, Signal, ['<?', Target, '?>'#10], False);
    end else begin
      if FOpenElementsCount > 0
        then WriteWideStrings(Reader, Signal, ['<?', Target, ' ', data, '?>'], False)
        else WriteWideStrings(Reader, Signal, ['<?', Target, ' ', data, '?>'#10], False);
    end;
  end;
  DoAfterWrite(xmlProcessingInstruction, Signal);
end;

procedure TXmlStreamBuilder.WriteSkippedEntitySignal(const Signal: TXmlSkippedEntitySignal);
begin
  // xxx not yet implemented.
end;

procedure TXmlStreamBuilder.WriteStartDocumentSignal(const Signal: TXmlStartDocumentSignal);
var
  NewCodecClass: TUnicodeCodecClass;
  NewEncName: WideString;
begin
  with Signal do begin
    FAttListDeclIsOpen := False;
    FOpenElementsCount := 0;
    FByteCount:= 0;
    FCharacterCount:= 0;
    FColumnCount:= 0;
    FLineFeedCount:= 0;
    FTabCount:= 0;
    NewEncName := EncodingName;
    if DefaultEncoding = '' then begin
      if NewEncName = ''
        then NewCodecClass := TUTF8Codec
        else NewCodecClass := StrToEncoding(NewEncName);
    end else begin
      NewEncName := DefaultEncoding;
      NewCodecClass := DefaultCodecClass;
    end;

    if Assigned(NewCodecClass) then begin

      DoBeforeWrite(xmlXmlDeclaration, Signal);
      PutCurrentCodecClass(NewCodecClass);
      FCurrentEncoding := NewEncName;

      WriteByteOrderMark(Reader, Signal, FByteCount);

      if IncludeXmlDecl then begin
        if Version = ''
          then WriteWideStrings(Reader, Signal, ['<?xml version="1.0"'], False)
          else WriteWideStrings(Reader, Signal, ['<?xml version="', Version, '"'], False);
        if CurrentEncoding <> '' then
          WriteWideStrings(Reader, Signal, [' encoding="', CurrentEncoding, '"'], False);
        case StandaloneDecl of
          STANDALONE_YES: WriteWideStrings(Reader, Signal, [' standalone="yes"'], False);
          STANDALONE_NO: WriteWideStrings(Reader, Signal, [' standalone="no"'], False);
        end;
        WriteWideStrings(Reader, Signal, ['?>'], False);
      end;
      DoAfterWrite(xmlXmlDeclaration, Signal);

    end else
      SendErrorNotification(Reader, ET_ENCODING_NOT_SUPPORTED, Signal, NewEncName);
  end;
end;

procedure TXmlStreamBuilder.WriteStartDocumentFragmentSignal(const Signal: TXmlStartDocumentFragmentSignal);
var
  NewCodecClass: TUnicodeCodecClass;
  NewEncName: WideString;
begin
  with Signal do begin
    FAttListDeclIsOpen := False;
    FOpenElementsCount := 0;
    FByteCount:= 0;
    FCharacterCount:= 0;
    FColumnCount:= 0;
    FLineFeedCount:= 0;
    FTabCount:= 0;
    NewEncName := EncodingName;

    if DefaultEncoding = '' then begin
      if NewEncName = ''
        then NewCodecClass := TUTF8Codec
        else NewCodecClass := StrToEncoding(NewEncName);
    end else begin
      NewEncName := DefaultEncoding;
      NewCodecClass := DefaultCodecClass;
    end;

    if Assigned(NewCodecClass) then begin
      DoBeforeWrite(xmlTextDeclaration, Signal);
      PutCurrentCodecClass(NewCodecClass);
      FCurrentEncoding := NewEncName;
      WriteByteOrderMark(Reader, Signal, FByteCount);
      DoAfterWrite(xmlTextDeclaration, Signal);
    end else
      SendErrorNotification(Reader, ET_ENCODING_NOT_SUPPORTED, Signal, NewEncName);
  end;
end;

procedure TXmlStreamBuilder.WriteStartElementSignal(const Signal: TXmlStartElementSignal);
var
  I: Integer;
begin
  DoBeforeWrite(xmlStartTag, Signal);
  with Signal do begin
    inc(FOpenElementsCount);
    WriteWideStrings(Reader, Signal, ['<', TagName], False);

    for I := 0 to pred(Attributes.Length) do begin
      WriteWideStrings(Reader, Signal, [' ', Attributes.Names[I], '="'], False);
      WriteWideStrings(Reader, Signal, [Attributes.values[I]], True);
      WriteWideStrings(Reader, Signal, ['"'], False);
    end;

    WriteWideStrings(Reader, Signal, ['>'], False);
  end;
  DoAfterWrite(xmlStartTag, Signal);
end;

procedure TXmlStreamBuilder.WriteAttributeDefinitionSignal(const Signal: TXmlAttributeDefinitionSignal);

  function XmlDataTypeToAttTypeStr(const DataType: TXmlDataType): wideString;
  begin
    case DataType of
      AS_STRING_DATATYPE:   Result := 'CDATA';
      AS_ID_DATATYPE:       Result := 'ID';
      AS_IDREF_DATATYPE:    Result := 'IDREF';
      AS_IDREFS_DATATYPE:   Result := 'IDREFS';
      AS_ENTITY_DATATYPE:   Result := 'ENTITY';
      AS_ENTITIES_DATATYPE: Result := 'ENTITIES';
      AS_NMTOKEN_DATATYPE:  Result := 'NMTOKEN';
      AS_NMTOKENS_DATATYPE: Result := 'NMTOKENS';
      AS_NOTATION_DATATYPE: Result := 'NOTATION';
    else
      raise EConvertError.Create('Datatype conversion not supported');;
    end;
  end;

const
  DQ: WideChar = #$22; // code of "
  SQ: WideChar = #$27; // code of '
var
  I: Integer;
begin
  with Signal do begin
    CheckAttListDeclarationOpen(Reader, Signal, ElementName);
    WriteWideStrings(Reader, Signal, ['          ', AttributeName, ' '], False);
    if Enumeration.Count > 0 then begin
      WriteWideStrings(Reader, Signal, ['('], False);
      for I := 0 to Pred(Enumeration.Count) do begin
        WriteWideStrings(Reader, Signal, [' ',Enumeration[I],' '], False);
        if I < Pred(Enumeration.Count) then
          WriteWideStrings(Reader, Signal, ['|'], False);
      end;
      WriteWideStrings(Reader, Signal, [') '], False);
    end else
      WriteWideStrings(Reader, Signal, [XmlDataTypeToAttTypeStr(AttributeType), ' '], False);
    case Constraint of
      AVC_FIXED:    WriteWideStrings(Reader, Signal, [' #FIXED'], False);
      AVC_IMPLIED:  WriteWideStrings(Reader, Signal, [' #IMPLIED'], False);
      AVC_REQUIRED: WriteWideStrings(Reader, Signal, [' #REQUIRED'], False);
    end;
    if Constraint in [AVC_DEFAULT, AVC_FIXED] then
      if Pos(DQ, DefaultValue) > 0
        then WriteWideStrings(Reader, Signal, [' ', SQ, DefaultValue, SQ, #10], False)
        else WriteWideStrings(Reader, Signal, [' ', DQ, DefaultValue, DQ, #10], False);
  end;
end;

procedure TXmlStreamBuilder.WriteElementTypeDeclarationSignal(const Signal: TXmlElementTypeDeclarationSignal);
begin
  DoBeforeWrite(xmlElementDecl, Signal);
  with Signal do begin
    WriteWideStrings(Reader, Signal, [#10'<!ELEMENT ', ElementName, ' ', Data, '>'], False);
  end;
  DoAfterWrite(xmlElementDecl, Signal);
end;

procedure TXmlStreamBuilder.WriteEntityDeclarationSignal(const Signal: TXmlEntityDeclarationSignal);
const
  SQ: WideChar = #39; // code of '
  DQ: WideChar = #34; // code of "
begin
  DoBeforeWrite(xmlEntityDecl, Signal);
  with Signal do begin
    WriteWideStrings(Reader, Signal, [#10'<!ENTITY ', EntityName, ' '], False);
    if ((PublicId = '') and (SystemId = '')) then begin
      if Pos(DQ, entityValue) > 0
        then WriteWideStrings(Reader, Signal, [SQ, EntityValue, SQ], False)
        else WriteWideStrings(Reader, Signal, [DQ, EntityValue, DQ], False);
    end else begin
      if PublicId = '' then begin
        if Pos(DQ, SystemId) > 0
          then WriteWideStrings(Reader, Signal, ['SYSTEM ', SQ, SystemId, SQ], False)
          else WriteWideStrings(Reader, Signal, ['SYSTEM ', DQ, SystemId, DQ], False);
      end else begin
        if SystemId = '' then begin
          WriteWideStrings(Reader, Signal, [' PUBLIC "', PublicId, '"'], False);
        end else begin
          if Pos(DQ, SystemId) > 0
            then WriteWideStrings(Reader, Signal, ['PUBLIC "', PublicId, '" ', SQ, SystemId, SQ], False)
            else WriteWideStrings(Reader, Signal, ['PUBLIC "', PublicId, '" "', SystemId, '"'], False);
        end;
      end; {if ...}
      if NotationName <> '' then
        WriteWideStrings(Reader, Signal, [' NDATA ', NotationName], False);
    end;
    WriteWideStrings(Reader, Signal, ['>'], False);
  end;
  DoAfterWrite(xmlEntityDecl, Signal);
end;

procedure TXmlStreamBuilder.WriteNotationDeclarationSignal(const Signal: TXmlNotationDeclarationSignal);
const
  SQ: WideChar = #39; // code of '
  DQ: WideChar = #34; // code of "
begin
  DoBeforeWrite(xmlNotationDecl, Signal);
  with Signal do begin
    WriteWideStrings(Reader, Signal, [#10'<!NOTATION ', NotationName, ' '], False);
    if PublicId = '' then begin
      if Pos(DQ, SystemId) > 0
        then WriteWideStrings(Reader, Signal, ['SYSTEM ', SQ, SystemId, SQ], False)
        else WriteWideStrings(Reader, Signal, ['SYSTEM ', DQ, SystemId, DQ], False);
    end else begin
      if SystemId = '' then begin
        WriteWideStrings(Reader, Signal, [' PUBLIC "',PublicId,'"'], False);
      end else begin
        if Pos(DQ, SystemId) > 0
          then WriteWideStrings(Reader, Signal, ['PUBLIC "', PublicId, '" ', SQ, SystemId, SQ], False)
          else WriteWideStrings(Reader, Signal, ['PUBLIC "', PublicId, '" "', SystemId, '"'], False);
      end;
    end; {if ...}
    WriteWideStrings(Reader, Signal, ['>'], False);
  end;
  DoAfterWrite(xmlNotationDecl, Signal);
end;

procedure TXmlStreamBuilder.WriteParameterEntityDeclarationSignal(const Signal: TXmlParameterEntityDeclarationSignal);
const
  SQ: WideChar = #39; // code of '
  DQ: WideChar = #34; // code of "
begin
  DoBeforeWrite(xmlParameterEntityDecl, Signal);
  with Signal do begin
    WriteWideStrings(Reader, Signal, [#10'<!ENTITY % ', entityName, ' '], False);
    if ((PublicId = '') and (SystemId = '')) then begin
      if Pos(DQ, entityValue) > 0
        then WriteWideStrings(Reader, Signal, [SQ, entityValue, SQ], False)
        else WriteWideStrings(Reader, Signal, [DQ, entityValue, DQ], False);
    end else begin
      if PublicId = '' then begin
        if Pos(DQ, SystemId) > 0
          then WriteWideStrings(Reader, Signal, ['SYSTEM ', SQ, SystemId, SQ], False)
          else WriteWideStrings(Reader, Signal, ['SYSTEM ', DQ, SystemId, DQ], False);
      end else begin
        if SystemId = '' then begin
          WriteWideStrings(Reader, Signal, [' PUBLIC "',PublicId,'"'], False);
        end else begin
          if Pos(DQ, SystemId) > 0
            then WriteWideStrings(Reader, Signal, ['PUBLIC "', PublicId, '" ', SQ, SystemId, SQ], False)
            else WriteWideStrings(Reader, Signal, ['PUBLIC "', PublicId, '" "', SystemId, '"'], False);
        end;
      end;
    end;
    WriteWideStrings(Reader, Signal, ['>'], False);
  end;
  DoAfterWrite(xmlParameterEntityDecl, Signal);
end;

procedure TXmlStreamBuilder.WriteStartExtDtdSignal(const Signal: TXmlStartExtDtdSignal);
var
  NewCodecClass: TUnicodeCodecClass;
  NewEncName: WideString;
begin
  with Signal do begin
    FAttListDeclIsOpen := False;
    FByteCount:= 0;
    FCharacterCount:= 0;
    FColumnCount:= 0;
    FLineFeedCount:= 0;
    FTabCount:= 0;

    NewEncName := EncodingName;
    if DefaultEncoding = '' then begin
      if NewEncName = ''
        then NewCodecClass := TUTF8Codec
        else NewCodecClass := StrToEncoding(NewEncName);
    end else begin
      NewEncName := DefaultEncoding;
      NewCodecClass := DefaultCodecClass;
    end;

    if assigned(NewCodecClass) then begin
      DoBeforeWrite(xmlTextDeclaration, Signal);

      PutCurrentCodecClass(NewCodecClass);

      WriteByteOrderMark(Reader, Signal, FByteCount);

      if IncludeXmlDecl then begin
        if Version = ''
          then WriteWideStrings(Reader, Signal, ['<?xml version="1.0"'], False)
          else WriteWideStrings(Reader, Signal, ['<?xml version="', Version, '"'], False);
        if NewEncName <> '' then
          WriteWideStrings(Reader, Signal, [' encoding="', NewEncName, '"'], False);
        WriteWideStrings(Reader, Signal, ['?>'#10], False);
      end;
      
      DoAfterWrite(xmlTextDeclaration, Signal);
    end else
      SendErrorNotification(Reader, ET_ENCODING_NOT_SUPPORTED, Signal, NewEncName);
  end;
end;

procedure TXmlStreamBuilder.WriteStartIntDtdSignal(const Signal: TXmlStartIntDtdSignal);
begin
// xxx To-Do: Add a parameter to TXmlStreamBuilder which controls the
//            following intializations:
//
//  FByteCount:= 0;
//  FCharacterCount:= 0;
//  FColumnCount:= 0;
//  FLineFeedCount:= 0;
//  FTabCount:= 0;
//

  ResetCurrentCodecClass;
  FAttListDeclIsOpen := False;
end;

procedure TXmlStreamBuilder.Reset;
begin
  ResetCurrentCodecClass;
  FAttListDeclIsOpen := False;
  FOpenElementsCount:= 0;
end;



// +++++++++++++++++++++++++ TXmlCustomReader ++++++++++++++++++++++++++
constructor TXmlCustomReader.Create(AOwner: TComponent);
begin
  inherited;
  FDOMImpl:= nil;
  FNextHandler:= nil;
end;

function TXmlCustomReader.getTabWidth: integer;
begin
  if Assigned(FDOMImpl)
    then Result := FDOMImpl.TabWidth
    else Result := 1;
end;

procedure TXmlCustomReader.SetDomImpl(const Impl: TDomImplementation);
begin
  if FDOMImpl = Impl then Exit;
  {$IFDEF VER140+}
  if Assigned(FDOMImpl)
    then FDOMImpl.RemoveFreeNotification(Self);
  {$ENDIF}
  {$IFDEF LINUX}
  if Assigned(FDOMImpl)
    then FDOMImpl.RemoveFreeNotification(Self);
  {$ENDIF}
  FDOMImpl := Impl;
  if Assigned(Impl)
    then Impl.FreeNotification(Self);
end;

procedure TXmlCustomReader.Notification(AComponent: TComponent;
                                        Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then begin
    if AComponent = FNextHandler then FNextHandler:= nil;
    if AComponent = FDOMImpl then FDOMImpl:= nil;
  end;
end;

procedure TXmlCustomReader.SendErrorNotification(const XmlErrorType: TXmlErrorType;
                                                 const Location: IDomLocator;
                                                 const Code: WideString);
var
  Error: TdomError;
  Ok: Boolean;
begin
  Error:= TdomError.CreateFromLocator(XmlErrorType, Location, Code);
  try
    if Assigned(FOnError) then
      FOnError(Self, Error);

    if Assigned(FDomImpl) then begin
      Ok := FDomImpl.HandleError(Self, Error);
    end else if Error.Severity = DOM_SEVERITY_FATAL_ERROR
      then Ok := False
      else Ok := True;

    if not Ok then
      raise EParserException.Create('Signal Processing Exception');
  finally
    Error.Free;
  end;
end;



// ++++++++++++++++++++++ TXmlStandardDocReader ++++++++++++++++++++++++
constructor TXmlStandardDocReader.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  suppressXmlns:= false;
  prefixMapping:= true;
  FPrefixMappingStack:= TList.Create;
end;

destructor TXmlStandardDocReader.destroy;
begin
  clearPrefixMappingStack;
  FPrefixMappingStack.free;
  inherited destroy;
end;

procedure TXmlStandardDocReader.clearPrefixMappingStack;
begin
  with FPrefixMappingStack do begin
    while count > 0 do begin
      TUtilsWideStringList(last).free;
      Delete(pred(count));
    end;
  end;
end;

procedure TXmlStandardDocReader.sendAbortedSignal(const locator: IDomLocator);
var
  XmlAbortedSignal: TXmlAbortedSignal;
begin
  ClearPrefixMappingStack;
  if Assigned(NextHandler) then begin
    XmlAbortedSignal := TXmlAbortedSignal.CreateFromLocator(Self, locator);
    try
      NextHandler.ProcessSignal(XmlAbortedSignal);
    finally
      XmlAbortedSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDocReader.writeCDATA(const locator: IDomLocator;
                                           const content: wideString);
var
  XmlCDATASignal: TXmlCDATASignal;
begin
  if Assigned(NextHandler) then begin
    XmlCDATASignal := TXmlCDATASignal.CreateFromLocator(Self, Locator);
    try
      XmlCDATASignal.Data := Content;
      NextHandler.processSignal(XmlCDATASignal);
    finally
      XmlCDATASignal.Free;
    end;
  end;
end;

procedure TXmlStandardDocReader.writeCharRefDec(const locator: IDomLocator;
                                                const content: wideString);
var
  XmlPCDATASignal: TXmlPCDATASignal;
  S: TUtilsCustomWideStr;
begin
  S:= TUtilsCustomWideStr.Create;
  try
    try
      S.AddUCS4Char(StrToInt64(content));

      if Assigned(NextHandler) then begin
        XmlPCDATASignal := TXmlPCDATASignal.CreateFromLocator(Self, Locator);
        try
          XmlPCDATASignal.CharRefGenerated := True;
          XmlPCDATASignal.Data := S.Value;
          NextHandler.processSignal(XmlPCDATASignal);
        finally
          XmlPCDATASignal.Free;
        end;
      end;

    except
      SendErrorNotification(ET_INVALID_CHARREF,locator,'&' + content + ';');
    end;
  finally
    S.Free;
  end;
end;

procedure TXmlStandardDocReader.writeCharRefHex(const locator: IDomLocator;
                                                const content: wideString);
var
  XmlPCDATASignal: TXmlPCDATASignal;
  S: TUtilsCustomWideStr;
begin
  S:= TUtilsCustomWideStr.Create;
  try
    try
      S.AddUCS4Char(StrToInt64(concat('$',content)));

      if Assigned(NextHandler) then begin
        XmlPCDATASignal := TXmlPCDATASignal.CreateFromLocator(Self, Locator);
        try
          XmlPCDATASignal.CharRefGenerated := True;
          XmlPCDATASignal.Data := S.Value;
          NextHandler.processSignal(XmlPCDATASignal);
        finally
          XmlPCDATASignal.Free;
        end;
      end;

    except
      SendErrorNotification(ET_INVALID_CHARREF,locator,'&x'+content+';');
    end;
  finally
    S.Free;
  end;
end;

procedure TXmlStandardDocReader.writeComment(const locator: IDomLocator;
                                             const content: wideString);
var
  XmlCommentSignal: TXmlCommentSignal;
begin
  if Assigned(NextHandler) then begin
    XmlCommentSignal := TXmlCommentSignal.CreateFromLocator(Self, Locator);
    try
      XmlCommentSignal.Data := Content;
      NextHandler.processSignal(XmlCommentSignal);
    finally
      XmlCommentSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDocReader.writePCDATA(const locator: IDomLocator;
                                            const content: wideString);
var
  XmlPCDATASignal: TXmlPCDATASignal;
begin
  if Assigned(NextHandler) then begin
    XmlPCDATASignal := TXmlPCDATASignal.CreateFromLocator(Self, Locator);
    try
      XmlPCDATASignal.Data := Content;
      NextHandler.processSignal(XmlPCDATASignal);
    finally
      XmlPCDATASignal.Free;
    end;
  end;
end;

procedure TXmlStandardDocReader.writeProcessingInstruction(const locator: IDomLocator;
                                                           const content: wideString);
var
  XmlProcessingInstructionSignal: TXmlProcessingInstructionSignal;
  TargetName,AttribSequence: wideString;
begin
  XMLAnalyseTag(content,TargetName,AttribSequence);  // xxx Replace by simpler procedure

  if Assigned(NextHandler) then begin
    XmlProcessingInstructionSignal := TXmlProcessingInstructionSignal.CreateFromLocator(Self, Locator);
    try
      XmlProcessingInstructionSignal.Target := TargetName;
      XmlProcessingInstructionSignal.Data := AttribSequence;
      NextHandler.processSignal(XmlProcessingInstructionSignal);
    finally
      XmlProcessingInstructionSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDocReader.writeStartDocument(const locator: IDomLocator;
                                                   const inputEnc,
                                                         version,
                                                         encName: wideString;
                                                         sdDl: TdomStandalone);
var
  XmlStartDocumentSignal: TXmlStartDocumentSignal;
begin
  clearPrefixMappingStack;

  if Assigned(NextHandler) then begin
    XmlStartDocumentSignal := TXmlStartDocumentSignal.CreateFromLocator(Self, Locator);
    try
      XmlStartDocumentSignal.InputEncoding := inputEnc;
      XmlStartDocumentSignal.Version := version;
      XmlStartDocumentSignal.EncodingName := encName;
      XmlStartDocumentSignal.StandaloneDecl := sdDl;
      NextHandler.processSignal(XmlStartDocumentSignal);
    finally
      XmlStartDocumentSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDocReader.writeStartDocumentFragment(const locator: IDomLocator;
                                                           const encName: wideString);
var
  XmlStartDocumentFragmentSignal: TXmlStartDocumentFragmentSignal;
begin
  clearPrefixMappingStack;

  if Assigned(NextHandler) then begin
    XmlStartDocumentFragmentSignal := TXmlStartDocumentFragmentSignal.CreateFromLocator(Self, Locator);
    try
      XmlStartDocumentFragmentSignal.EncodingName := encName;
      NextHandler.processSignal(XmlStartDocumentFragmentSignal);
    finally
      XmlStartDocumentFragmentSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDocReader.writeStartElement(const locator: IDomLocator;
                                                  const content: wideString);
var
  XmlStartElementSignal: TXmlStartElementSignal;
  TagName: wideString;
begin
  if Assigned(NextHandler) then begin
    XmlStartElementSignal := TXmlStartElementSignal.CreateFromLocator(Self, Locator);
    try
      if FPrefixMapping or FSuppressXmlns
        then WriteStartElementXmlns(locator, content, tagName, XmlStartElementSignal.Attributes)    // xxx Replace individual parameters with TXmlStartElementSignal object parameter.
        else WriteStartElementSimple(locator, content, tagName, XmlStartElementSignal.Attributes);  // xxx Ditto.
      XmlStartElementSignal.TagName := TagName;
      NextHandler.ProcessSignal(XmlStartElementSignal);
    finally
      XmlStartElementSignal.Free;
    end;
  end; {if ...}
end;

procedure TXmlStandardDocReader.writeStartElementSimple(const locator: IDomLocator;
                                                              content: wideString;
                                                          out tagName: wideString;
                                                        const attrList: TUtilsNameValueList);
const
  NULL:  WideChar = #0; // end of wideString mark
  SQ:    WideChar = #39;  // code of '
  DQ:    WideChar = #34;  // code of "
  EQ:    WideChar = #61;  // code of =
var
  dummyIndex: integer;
  head,tail: PWideChar;
  attrName,attrValue: wideString;
  quotationMark: WideChar;
begin
  attrList.sorted:= true;

  // Find tag name:
  head:= PWideChar(content);
  tail:= head;
  while not isXmlWhiteSpaceOrNull(tail^) do
    inc(tail);
  setString(tagName,head,tail-head);

  // Skip white space:
  head:= tail;
  while IsXmlWhiteSpace(head^) do
    inc(head);

  while head^ <> NULL do begin

    // Find next attribute name:
    tail:= head;
    while not IsXmlWhiteSpace(tail^) and not (tail^ in [NULL,EQ]) do begin
      inc(tail);
    end;
    setString(attrName,head,tail-head);

    // Find equation sign and quotation mark:
    head:= tail;
    while IsXmlWhiteSpace(head^) do
      inc(head);
    if head^ <> EQ then begin
      sendErrorNotification(ET_MISSING_EQUALITY_SIGN,locator,'');
      exit;
    end;
    inc(head);
    while IsXmlWhiteSpace(head^) do
      inc(head);
    if not (head^ in [SQ,DQ]) then begin
      sendErrorNotification(ET_MISSING_QUOTATION_MARK,locator,'');
      exit;
    end;
    quotationMark:= WideChar(head^);
    inc(head);
    tail:= head;

    // Find next attribute value:
    while not (tail^ in [NULL,quotationMark]) do
      inc(tail);
    if tail^ = NULL then begin
      sendErrorNotification(ET_MISSING_QUOTATION_MARK,locator,'');
      exit;
    end;
    setString(attrValue,head,tail-head);

    // Evaluate what has been found:
    if attrList.findOfName(attrName,dummyIndex) then begin
      sendErrorNotification(ET_DOUBLE_ATTRIBUTE_NAME,locator,'');
      exit;
    end;
    attrList.add(attrName,attrValue);

    // Skip white space:
    head:= tail;
    inc(head);
    if not IsXmlWhiteSpaceOrNull(head^) then begin
      sendErrorNotification(ET_MISSING_WHITE_SPACE,locator,'');
      exit;
    end;
    while IsXmlWhiteSpace(head^) do
      inc(head);
  end; {while ...}
end;

procedure TXmlStandardDocReader.writeStartElementXmlns(const locator: IDomLocator;
                                                             content: wideString;
                                                         out tagName: wideString;
                                                       const attrList: TUtilsNameValueList);
const
  NULL:  WideChar = #0; // end of wideString mark
  SQ:    WideChar = #39;  // code of '
  DQ:    WideChar = #34;  // code of "
  EQ:    WideChar = #61;  // code of =
  XMLNS: array[1..6] of WideChar = ('x', 'm', 'l', 'n', 's', ':');
var
  attrName: wideString;
  attrNameLength: integer;
  attrValue: wideString;
  dummyIndex: integer;
  isXmlns: boolean;
  head: PWideChar;
  namespacePrefix: wideString;
  pfxUriList: TUtilsWideStringList;
  quotationMark: WideChar;
  tail: PWideChar;
begin
  attrList.sorted:= true;
  if FPrefixMapping then begin
    pfxUriList:= TUtilsWideStringList.create;
    FPrefixMappingStack.Add(pfxUriList);
  end;

  // Find tag name:
  head:= PWideChar(content);
  tail:= head;
  while not IsXmlWhiteSpaceOrNull(tail^) do
    inc(tail);
  setString(tagName,head,tail-head);

  // Skip white space:
  head:= tail;
  while IsXmlWhiteSpace(head^) do
    inc(head);

  while head^ <> NULL do begin

    // Find next attribute name:
    isXmlns:= true;
    tail:= head;
    attrNameLength:= 0;
    while not IsXmlWhiteSpace(tail^) and not (tail^ in [NULL,EQ]) do begin
      inc(attrNameLength);
      if attrNameLength <= 6
        then if tail^ <> XMLNS[attrNameLength]
          then isXmlns:= false;
      inc(tail);
    end;
    setString(attrName,head,attrNameLength);

    // Evaluate namespace prefix:
    if isXmlns then begin
      if attrNameLength > 6 then begin  // tag has the form 'xmlns:...'.
        setString(namespacePrefix,head+6,attrNameLength-6);
      end else if attrNameLength = 5 then begin // tag has the form 'xmlns'.
        namespacePrefix:= '';
      end else if (attrNameLength < 5) or (attrNameLength = 6) then begin // tag has either the form 'x', 'xm', 'xml', 'xmln', or 'xmlns:'.
        isXmlns:= false;
      end;
    end;

    // Find equation sign and quotation mark:
    head:= tail;
    while IsXmlWhiteSpace(head^) do
      inc(head);
    if head^ <> EQ then begin
      sendErrorNotification(ET_MISSING_EQUALITY_SIGN,locator,'');
      exit;
    end;
    inc(head);
    while IsXmlWhiteSpace(head^) do
      inc(head);
    if not (head^ in [SQ,DQ]) then begin
      sendErrorNotification(ET_MISSING_QUOTATION_MARK,locator,'');
      exit;
    end;
    quotationMark:= WideChar(head^);
    inc(head);
    tail:= head;

    // Find next attribute value:
    while not (tail^ in [NULL,quotationMark]) do
      inc(tail);
    if tail^ = NULL then begin
      sendErrorNotification(ET_MISSING_QUOTATION_MARK,locator,'');
      exit;
    end;
    setString(attrValue,head,tail-head);

    // Evaluate what has been found:
    if attrList.findOfName(attrName,dummyIndex) then begin
      sendErrorNotification(ET_DOUBLE_ATTRIBUTE_NAME,locator,'');
      exit;
    end;
    if isXmlns then begin
      if FPrefixMapping then begin  // xxx Use special writeStartElementXmlns2 functions for different values of FPrefixMapping?
        pfxUriList.add(namespacePrefix);
        writeStartPrefixMapping(locator,xmlExtractLocalName(attrName),attrValue);
      end;
      if not FSuppressXmlns
        then attrList.add(attrName,attrValue);
    end else attrList.add(attrName,attrValue);

    // Skip white space:
    head:= tail;
    inc(head);
    if not IsXmlWhiteSpaceOrNull(head^) then begin
      sendErrorNotification(ET_MISSING_WHITE_SPACE,locator,'');
      exit;
    end;
    while IsXmlWhiteSpace(head^) do
      inc(head);
  end; {while ...}
end;

procedure TXmlStandardDocReader.writeStartPrefixMapping(const locator: IDomLocator;
                                                        const prefix,
                                                              uri: wideString);
var
  XmlStartPrefixMappingSignal: TXmlStartPrefixMappingSignal;
begin
  if Assigned(NextHandler) then begin
    XmlStartPrefixMappingSignal := TXmlStartPrefixMappingSignal.CreateFromLocator(Self, Locator);
    try
      XmlStartPrefixMappingSignal.Prefix := prefix;
      XmlStartPrefixMappingSignal.Uri := uri;
      NextHandler.processSignal(XmlStartPrefixMappingSignal);
    finally
      XmlStartPrefixMappingSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDocReader.writeEndElement(const locator: IDomLocator;
                                                const content: wideString);
var
  XmlEndElementSignal: TXmlEndElementSignal;
begin
  if Assigned(NextHandler) then begin
    XmlEndElementSignal := TXmlEndElementSignal.CreateFromLocator(Self, Locator);
    try
      XmlEndElementSignal.TagName := content;
      NextHandler.processSignal(XmlEndElementSignal);
      writeEndPrefixMapping(locator);
    finally
      XmlEndElementSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDocReader.writeEndPrefixMapping(const locator: IDomLocator);
var
  XmlEndPrefixMappingSignal: TXmlEndPrefixMappingSignal;
  pfxUriList: TUtilsWideStringList;
  i: integer;
begin
  if assigned(NextHandler) then
    if FPrefixMapping then
      with FPrefixMappingStack do 
        if count > 0 then begin
          pfxUriList:= last;
          Delete(pred(count));
          try
            with pfxUriList do
              for i:= pred(count) downto 0 do begin
                XmlEndPrefixMappingSignal := TXmlEndPrefixMappingSignal.CreateFromLocator(Self, Locator);
                try
                  XmlEndPrefixMappingSignal.Prefix := WideStrings[i];
                  NextHandler.processSignal(XmlEndPrefixMappingSignal);
                finally
                  XmlEndPrefixMappingSignal.Free;
                end;
              end;
          finally
            pfxUriList.free;
          end;
        end; {if ...}
end;

procedure TXmlStandardDocReader.writeEmptyElement(const locator: IDomLocator;
                                                  const content: wideString);
var
  XmlStartElementSignal: TXmlStartElementSignal;
  TagName: wideString;
begin
  if Assigned(NextHandler) then begin
    XmlStartElementSignal := TXmlStartElementSignal.CreateFromLocator(Self, Locator);
    try
      if FPrefixMapping or FSuppressXmlns
        then WriteStartElementXmlns(Locator, Content, TagName, XmlStartElementSignal.Attributes)
        else WriteStartElementSimple(Locator, Content, TagName, XmlStartElementSignal.Attributes);
      XmlStartElementSignal.TagName := TagName;
      NextHandler.processSignal(XmlStartElementSignal);
      WriteEndElement(Locator, TagName);
    finally
      XmlStartElementSignal.Free;
    end;
  end; {if ...}
end;

procedure TXmlStandardDocReader.writeEntityRef(const locator: IDomLocator;
                                               const content: wideString);
var
  XmlEntityRefSignal: TXmlEntityRefSignal;
begin
  if Assigned(NextHandler) then begin
    XmlEntityRefSignal := TXmlEntityRefSignal.CreateFromLocator(Self, Locator);
    try
      XmlEntityRefSignal.EntityName := content;
      NextHandler.processSignal(XmlEntityRefSignal);
    finally
      XmlEntityRefSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDocReader.writeDoctype(const locator: IDomLocator;
                                             const content: wideString);

const
  LF  = $A; // Line Feed
  TAB = $9; // Tab
var
  DoctypeDeclTokenizer: TXmlDoctypeDeclTokenizer;
  DoctypeName: wideString;
  InputSrc: TXmlInputSource;
  IntDtdByteNumber, IntDtdCharNumber, IntDtdLine, IntDtdColumn: integer;
  IntDtd: WideString;
  PubidLiteral: WideString;
  S: WideString;
  StrStream : TUtilsWideStringStream;
  SystemLiteral: WideString;
  XmlDoctypeSignal: TXmlDoctypeSignal;
begin
  IntDtdByteNumber := 0;
  IntDtdCharNumber := 0;
  IntDtdLine := 1;
  IntDtdColumn := 0;

  S := Concat(WideString(#$FEFF), Content); // Add byte order mark.
  StrStream := TUtilsWideStringStream.Create(S);
  try
    with Locator do
      if StartColumnNumber = 0  // Indicates a starting LF
        then InputSrc := TXmlInputSource.Create(StrStream, '', URI, 4096,
                    'UTF-16LE', False, StartByteNumber, StartCharNumber - 1,
                    StartColumnNumber - 1, 0, StartLineNumber - 1)
        else InputSrc := TXmlInputSource.Create(StrStream, '', URI, 4096,
                    'UTF-16LE', False, StartByteNumber, StartCharNumber - 1,
                    StartColumnNumber - 1, 0, StartLineNumber);
    try
      DoctypeDeclTokenizer := TXmlDoctypeDeclTokenizer.Create(InputSrc, TabWidth);
      try
        with DoctypeDeclTokenizer do begin
          while TokenType <> DOCTYPE_END_OF_SOURCE_TOKEN do begin
            Next;
            if ErrorType <> ET_NONE then begin
              SendErrorNotification(ErrorType, DoctypeDeclTokenizer, TokenValue); // xxx evaluate DoctypeDeclTokenizer.Clue !!!
              Exit;
            end;
            case tokenType of
              DOCTYPE_NAME_TOKEN:
                DoctypeName := TokenValue;
              DOCTYPE_PUBID_TOKEN:
                PubidLiteral := TokenValue;
              DOCTYPE_SYSID_TOKEN:
                SystemLiteral := TokenValue;
              DOCTYPE_INTSUBSET_TOKEN:
                begin
                  IntDtdByteNumber := DoctypeDeclTokenizer.GetStartByteNumber;
                  IntDtdCharNumber := DoctypeDeclTokenizer.GetStartCharNumber - 1;
                  IntDtdColumn := DoctypeDeclTokenizer.GetStartColumnNumber - 1;
                  IntDtdLine := DoctypeDeclTokenizer.GetStartLineNumber;
                  if IntDtdColumn = -1 then  // Indicates a starting LF
                    Dec(IntDtdLine);
                  IntDtd := TokenValue;
                end;
            end;
          end;
        end;
      finally
        DoctypeDeclTokenizer.Free;
      end;
    finally
      InputSrc.Free;
    end;
  finally
    StrStream.Free;
  end;

  if Assigned(NextHandler) then begin
    XmlDoctypeSignal := TXmlDoctypeSignal.CreateFromLocator(Self, Locator);
    try
      XmlDoctypeSignal.DoctypeName := DoctypeName;
      XmlDoctypeSignal.PublicId := PubidLiteral;
      XmlDoctypeSignal.SystemId := SystemLiteral;
      XmlDoctypeSignal.Data := IntDtd;
      XmlDoctypeSignal.IntSubsetStartByteNumber := IntDtdByteNumber;
      XmlDoctypeSignal.IntSubsetStartCharNumber := IntDtdCharNumber;
      XmlDoctypeSignal.IntSubsetStartColumn := IntDtdColumn;
      XmlDoctypeSignal.IntSubsetStartLine := IntDtdLine;
      NextHandler.processSignal(XmlDoctypeSignal);
    finally
      XmlDoctypeSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDocReader.writeCompleted(const locator: IDomLocator);
var
  XmlCompletedSignal: TXmlCompletedSignal;
begin
  ClearPrefixMappingStack;
  if Assigned(NextHandler) then begin
    XmlCompletedSignal := TXmlCompletedSignal.CreateFromLocator(Self, Locator);
    try
      NextHandler.ProcessSignal(XmlCompletedSignal);
    finally
      XmlCompletedSignal.Free;
    end;
  end;
end;

function TXmlStandardDocReader.parse(const inputSource: TXmlInputSource): boolean;
var
  XmlTokenizer: TXmlDocTokenizer;
begin
  XmlTokenizer:= TXmlDocTokenizer.create(InputSource, TabWidth);
  try
    Result := True;
    try
      with InputSource do
        WriteStartDocument(XmlTokenizer, InputEncoding, XmlVersion, XmlEncoding, XmlStandalone);
      Parse2(XmlTokenizer);
      WriteCompleted(XmlTokenizer);
    except
      SendAbortedSignal(XmlTokenizer);
      Result := False;
    end; {try ...}
  finally
    XmlTokenizer.free;
  end;
end;

function TXmlStandardDocReader.parseFragment(const inputSource: TXmlInputSource): boolean;
var
  XmlTokenizer: TXmlDocTokenizer;
begin
  XmlTokenizer:= TXmlDocTokenizer.create(InputSource, TabWidth);
  try
    Result := True;
    try
      WriteStartDocumentFragment(XmlTokenizer, InputSource.XmlEncoding);
      Parse2(XmlTokenizer);
      WriteCompleted(XmlTokenizer);
    except
      SendAbortedSignal(XmlTokenizer);
      Result := False;
    end; {try ...}
  finally
    XmlTokenizer.free;
  end;
end;

procedure TXmlStandardDocReader.parse2(const xmlTokenizer: TXmlDocTokenizer);
begin
  with XmlTokenizer do begin
    while tokenType <> XML_END_OF_SOURCE_TOKEN do begin
      next;
      if errorType <> ET_NONE then
        sendErrorNotification(errorType, XmlTokenizer, tokenValue); // xxx evaluate XmlTokenizer.clue !!!
      // For speed optimization, the case statements are ordered according to
      // what I guess is their frequency in a typical XML document.
      case tokenType of
        XML_PCDATA_TOKEN:            writePCDATA(XmlTokenizer, tokenValue);
        XML_START_TAG_TOKEN:         writeStartElement(XmlTokenizer, tokenValue);
        XML_END_TAG_TOKEN:           writeEndElement(XmlTokenizer, tokenValue);
        XML_ENTITY_REF_TOKEN:        writeEntityRef(XmlTokenizer, tokenValue);
        XML_EMPTY_ELEMENT_TAG_TOKEN: writeEmptyElement(XmlTokenizer, tokenValue);
        XML_CHAR_REF_HEX_TOKEN:      writeCharRefHex(XmlTokenizer, tokenValue);
        XML_CHAR_REF_DEC_TOKEN:      writeCharRefDec(XmlTokenizer, tokenValue);
        XML_COMMENT_TOKEN:           writeComment(XmlTokenizer, tokenValue);
        XML_PI_TOKEN:                writeProcessingInstruction(XmlTokenizer, tokenValue);
        XML_CDATA_TOKEN:             writeCDATA(XmlTokenizer, tokenValue);
        XML_DOCTYPE_TOKEN:           writeDoctype(XmlTokenizer, tokenValue);
      end;
    end;
  end;
end;



// +++++++++++++++++++++ TXmlStandardDtdReader +++++++++++++++++++++
constructor TXmlStandardDtdReader.create(AOwner: TComponent);
begin
  inherited;
  FPERefTreatment := PT_PARSE;
  FAttrListDeclNames := TUtilsWideStringList.Create;
  FElementTypeDeclNames := TUtilsWideStringList.Create;
  FPERepository := TdomPERepository.Create(Self);
end;

destructor TXmlStandardDtdReader.destroy;
begin
  FPERepository.Free;
  FAttrListDeclNames.Free;
  FElementTypeDeclNames.Free;
  inherited;
end;

function TXmlStandardDtdReader.findNextAttDef(    decl: wideString;
                                              out attType: TXmlDataType;
                                              out constraint: TdomAttrValueConstraint;
                                              out attName,
                                                  enumeration,
                                                  defaultValue,
                                                  rest: wideString): boolean;
// Return value: 'false' if a wellformedness error occured; 'true' otherwise.

  function StrToDataType(const S: wideString;
                           out DataType: TXmlDataType): Boolean;
  begin
    if S = '' then begin
      DataType:= AS_NMTOKEN_DATATYPE;
      Result := True;
    end else if S = 'CDATA' then begin
      DataType:= AS_STRING_DATATYPE;
      Result := True;
    end else if S = 'ID' then begin
      DataType:= AS_ID_DATATYPE;
      Result := True;
    end else if S = 'IDREF' then begin
      DataType:= AS_IDREF_DATATYPE;
      Result := True;
    end else if S = 'IDREFS' then begin
      DataType:= AS_IDREFS_DATATYPE;
      Result := True;
    end else if S = 'ENTITY' then begin
      DataType:= AS_ENTITY_DATATYPE;
      Result := True;
    end else if S = 'ENTITIES' then begin
      DataType:= AS_ENTITIES_DATATYPE;
      Result := True;
    end else if S = 'NMTOKEN' then begin
      DataType:= AS_NMTOKEN_DATATYPE;
      Result := True;
    end else if S = 'NMTOKENS' then begin
      DataType:= AS_NMTOKENS_DATATYPE;
      Result := True;
    end else if S = 'NOTATION' then begin
      DataType:= AS_NOTATION_DATATYPE;
      Result := True;
    end else
      Result := False;
  end;

  function StrToConstaintType(const S: wideString;
                                var AVC: TdomAttrValueConstraint): Boolean;
  begin
    if S = '#REQUIRED' then begin
      AVC := AVC_REQUIRED;
      Result := True;
    end else if S = '#IMPLIED' then begin
      AVC := AVC_IMPLIED;
      Result := True;
    end else if S = '#FIXED' then begin
      AVC := AVC_FIXED;
      Result := True;
    end else
      Result := False;
  end;

var
  i,j: integer;
  FindEnumeration, FindConstraint, FindDefaultValue: boolean;
  QuoteType: WideChar;
  S: wideString;
begin
  result:= true;

  S:= '';
  defaultValue:= '';
  Enumeration:= '';
  FindDefaultValue:= false;
  FindEnumeration:= false;
  FindConstraint:= false;
  constraint:= AVC_DEFAULT;
  attName:= '';
  rest:= '';

  if Length(Decl) = 0
    then begin result:= false; exit; end;
  i:= 1;

  {White-space?}
  while IsXmlWhiteSpace(Decl[i]) do begin
    inc(i);
    if i > length(Decl)
      then begin result:= false; exit; end;
  end;
  j:= i;

  {AttName?}
  while not IsXmlWhiteSpace(Decl[i]) do begin
    inc(i);
    if i > length(Decl)
      then begin result:= false; exit; end;
  end;
  attName:= copy(Decl,j,i-j);

  {White-space?}
  while IsXmlWhiteSpace(Decl[i]) do begin
    inc(i);
    if i > length(Decl)
      then begin result:= false; exit; end;
  end;
  j:= i;

  if Decl[j] = '(' then FindEnumeration:= true;

  {AttType?}
  if FindEnumeration then begin
    attType := AS_NMTOKEN_DATATYPE;
  end else begin
    while not IsXmlWhiteSpace(Decl[i]) do begin
      inc(i);
      if i > length(Decl)
        then begin result:= false; exit; end;
    end;
    if not StrToDataType(copy(Decl,j,i-j), attType)
      then begin result:= false; exit; end;
    if attType = AS_NOTATION_DATATYPE then
      FindEnumeration:= true;

    {White-space?}
    while IsXmlWhiteSpace(Decl[i]) do begin
      inc(i);
      if i > length(Decl)
        then begin result:= false; exit; end;
    end;
    j:= i;
  end; {if ...}

  {Bracket?}
  if FindEnumeration then begin
    if Decl[j] <> '('
      then begin result:= false; exit; end;
    while not (Decl[i] = ')') do begin
      inc(i);
      if i >= length(Decl)
        then begin result:= false; exit; end;
    end;
    Enumeration:= copy(Decl,j,i-j+1);

    {White-space?}
    inc(i);
    if not IsXmlWhiteSpace(Decl[i])
      then begin result:= false; exit; end;
    while IsXmlWhiteSpace(Decl[i]) do begin
      inc(i);
      if i > length(Decl)
        then begin result:= false; exit; end;
    end;
    j:= i;
  end; {if ...}

  if Decl[j] = '#'
    then FindConstraint:= true
    else FindDefaultValue:= true;

  {Constraint?}
  if FindConstraint then begin
    while not IsXmlWhiteSpace(Decl[i]) do begin
      inc(i);
      if i > length(Decl) then break;
    end; {while ...}
    if not  StrToConstaintType(copy(Decl,j,i-j), constraint)
      then begin result:= false; exit; end;
    if constraint = AVC_FIXED then begin
      FindDefaultValue:= true;
      {White-space?}
      if i > length(Decl)
        then begin result:= false; exit; end;
      while IsXmlWhiteSpace(Decl[i]) do begin
        inc(i);
        if i > length(Decl)
          then begin result:= false; exit; end;
      end; {while ...}
      j:= i;
    end; {if ...}
  end; {if ...}

  {DefaultValue?}
  if FindDefaultValue then begin
    if i = length(Decl)
      then begin result:= false; exit; end;
    QuoteType:= Decl[i];
    if not ( (QuoteType = '"') or (QuoteType = #$0027))
      then begin result:= false; exit; end;
    inc(i);
    while not (Decl[i] = QuoteType) do begin
      inc(i);
      if i > length(Decl)
        then begin result:= false; exit; end;
    end; {while ...}
    defaultValue:= copy(Decl,j+1,i-j-1);
    inc(i);
  end; {if ...}

  Rest:= copy(Decl,i,length(Decl)-i+1);
end;

procedure TXmlStandardDtdReader.sendAbortedSignal(const locator: IDomLocator);
var
  XmlAbortedSignal: TXmlAbortedSignal;
begin
  if Assigned(NextHandler) then begin
    XmlAbortedSignal := TXmlAbortedSignal.CreateFromLocator(Self, locator);
    try
      NextHandler.ProcessSignal(XmlAbortedSignal);
    finally
      XmlAbortedSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDtdReader.sendErrorNotification(
  const xmlErrorType: TXmlErrorType; const location: IDomLocator;
  const code: wideString);
begin
  if xmlErrorType in (ET_FATAL_ERRORS + ET_ERRORS) then
    FXmlErrorDetected := True;
  inherited;
end;

procedure TXmlStandardDtdReader.WriteAttributeDeclaration(const locator: IDomLocator;
                                                          const elementName: wideString;
                                                                data: wideString);
var
  dummy,piece: wideString;
  separator: integer;
  Error: boolean;
  XmlAttributeDefinitionSignal: TXmlAttributeDefinitionSignal;
  attType: TXmlDataType;
  constraint: TdomAttrValueConstraint;
  AttDefName, enum1, enum2, defaultValue, Rest: wideString;
begin
  if not IsXmlName(ElementName) then
    SendErrorNotification(ET_INVALID_ATTLIST_DECL_NAME, Locator, ElementName);
    // Remark: This test is necessary here, because if 'data' is empty, no
    //         further tests in a processor pipeline will be carried out.

  // Keep track of the element types of attribute-list declarations and warn the
  // application when detecting a duplicate:
  if FAttrListDeclNames.IndexOf(ElementName) = -1
    then FAttrListDeclNames.Add(ElementName)
    else SendErrorNotification(ET_DOUBLE_ATTLISTDECL, Locator, ElementName);

  if Assigned(NextHandler) then begin

    while Data <> '' do begin

      if FindNextAttDef(Data, attType, constraint, AttDefName, enum1, defaultValue, Rest) then begin
        XmlAttributeDefinitionSignal := TXmlAttributeDefinitionSignal.CreateFromLocator(Self, Locator);
        try
          XmlAttributeDefinitionSignal.AttributeName := attDefName;
          XmlAttributeDefinitionSignal.AttributeType := attType;
          XmlAttributeDefinitionSignal.Constraint := constraint;
          XmlAttributeDefinitionSignal.DefaultValue := defaultValue;
          XmlAttributeDefinitionSignal.ElementName := elementName;

          // Process enumeration of attributes:
          if enum1 <> '' then begin
            XMLTruncRoundBrackets(enum1, enum2, Error);
            if Error or (enum2 = '') then begin
              SendErrorNotification(ET_INVALID_ATTRIBUTE_DECL, Locator, Data);
              Exit;
            end;
            while enum2 <> '' do begin
              Separator := Pos(wideString('|'), enum2);
              if Separator = 0 then begin
                Piece := enum2;
                enum2 := '';
              end else begin
                Piece := trimWhitespace(copy(enum2,1,separator-1));
                dummy := trimWhitespace(copy(enum2,separator+1,length(enum2)-separator));
                enum2 := dummy;
                if enum2 = '' then begin
                  SendErrorNotification(ET_INVALID_ATTRIBUTE_DECL, Locator, Data);
                  Exit;
                end;
              end;
              XmlAttributeDefinitionSignal.Enumeration.add(Piece);
            end; {while ...}
          end;

          NextHandler.processSignal(XmlAttributeDefinitionSignal);
        finally
          XmlAttributeDefinitionSignal.Free;
        end;
      end else begin
        SendErrorNotification(ET_INVALID_ATTRIBUTE_DECL, Locator, Data);
        Exit;
      end;

      Data := Rest;

    end; {while ...}

  end;
end;

procedure TXmlStandardDtdReader.WriteDTDComment(const locator: IDomLocator;
                                               const data: wideString);
var
  XmlCommentSignal: TXmlCommentSignal;
begin
  if Assigned(NextHandler) then begin
    XmlCommentSignal := TXmlCommentSignal.CreateFromLocator(Self, Locator);
    try
      XmlCommentSignal.Data := data;
      NextHandler.processSignal(XmlCommentSignal);
    finally
      XmlCommentSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDtdReader.WriteDTDProcessingInstruction(const locator: IDomLocator;
                                                             const target,
                                                                   data: wideString);
var
  XmlProcessingInstructionSignal: TXmlProcessingInstructionSignal;
begin
  if Assigned(NextHandler) then begin
    XmlProcessingInstructionSignal := TXmlProcessingInstructionSignal.CreateFromLocator(Self, Locator);
    try
      XmlProcessingInstructionSignal.Target := target;
      XmlProcessingInstructionSignal.Data := data;
      NextHandler.processSignal(XmlProcessingInstructionSignal);
    finally
      XmlProcessingInstructionSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDtdReader.WriteElementDeclaration(const locator: IDomLocator;
                                                       const elementName,
                                                             data: wideString);
var
  XmlElementTypeDeclarationSignal: TXmlElementTypeDeclarationSignal;
begin
  // Keep track of the element type declarations and report an error to the
  // application when detecting a duplicate:
  if FElementTypeDeclNames.IndexOf(ElementName) = -1 then begin
    FElementTypeDeclNames.Add(ElementName)
  end else begin
    // VC: Unique Element Type Declaration (XML 1.0, § 3.2)
    SendErrorNotification(ET_DUPLICATE_ELEMENT_TYPE_DECL, Locator, ElementName);
    Exit;
  end;

  if Assigned(NextHandler) then begin
    XmlElementTypeDeclarationSignal := TXmlElementTypeDeclarationSignal.CreateFromLocator(Self, Locator);
    try
      XmlElementTypeDeclarationSignal.ElementName := elementName;
      XmlElementTypeDeclarationSignal.Data := data;
      NextHandler.processSignal(XmlElementTypeDeclarationSignal);
    finally
      XmlElementTypeDeclarationSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDtdReader.WriteEntityDeclaration(const locator: IDomLocator;
                                                      const entityName,
                                                            data: wideString);
var
  XmlEntityDeclarationSignal: TXmlEntityDeclarationSignal;
  EntityValue, SystemLiteral, PubidLiteral, NDataName: wideString;
  Error: boolean;
begin
  xmlAnalyseEntityDef(data, EntityValue, SystemLiteral, PubidLiteral, NDataName, Error);
  if Error then begin
    SendErrorNotification(ET_INVALID_ENTITY_DECL, Locator, '');
    Exit;
  end;

  if Assigned(NextHandler) then begin
    XmlEntityDeclarationSignal := TXmlEntityDeclarationSignal.CreateFromLocator(Self, Locator);
    try
      XmlEntityDeclarationSignal.EntityName := entityName;
      XmlEntityDeclarationSignal.EntityValue := entityValue;
      XmlEntityDeclarationSignal.PublicId := PubidLiteral;
      XmlEntityDeclarationSignal.SystemId := SystemLiteral;
      XmlEntityDeclarationSignal.NotationName := NDataName;
      NextHandler.processSignal(XmlEntityDeclarationSignal);
    finally
      XmlEntityDeclarationSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDtdReader.WriteCompleted(const locator: IDomLocator);
var
  XmlCompletedSignal: TXmlCompletedSignal;
begin
  if Assigned(NextHandler) then begin
    XmlCompletedSignal := TXmlCompletedSignal.CreateFromLocator(Self, Locator);
    try
      NextHandler.ProcessSignal(XmlCompletedSignal);
    finally
      XmlCompletedSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDtdReader.WriteNotationDeclaration(const locator: IDomLocator;
                                                        const notationName: wideString;
                                                              data: wideString);
var
  XmlNotationDeclarationSignal: TXmlNotationDeclarationSignal;
  SystemLiteral, PubidLiteral: wideString;
  Error: boolean;
begin
  XMLAnalyseNotationDecl(data, SystemLiteral, PubidLiteral, Error);
  if Error then begin
    SendErrorNotification(ET_INVALID_NOTATION_DECL,locator,'');
    Exit;
  end;

  if Assigned(NextHandler) then begin
    XmlNotationDeclarationSignal := TXmlNotationDeclarationSignal.CreateFromLocator(Self, Locator);
    try
      XmlNotationDeclarationSignal.NotationName := notationName;
      XmlNotationDeclarationSignal.PublicId := PubidLiteral;
      XmlNotationDeclarationSignal.SystemId := SystemLiteral;
      NextHandler.processSignal(XmlNotationDeclarationSignal);
    finally
      XmlNotationDeclarationSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDtdReader.WriteParameterEntityDeclaration(const locator: IDomLocator;
                                                               const entityName,
                                                                     data: wideString);
var
  XmlParameterEntityDeclarationSignal: TXmlParameterEntityDeclarationSignal;
  EntityValue, SystemLiteral, PubidLiteral, NDataDummy: wideString;
  Error: boolean;
begin
  xmlAnalyseEntityDef(data, EntityValue, SystemLiteral, PubidLiteral, NDataDummy, Error);
  if Error or (NDataDummy <> '') then begin
    SendErrorNotification(ET_INVALID_PARAMETER_ENTITY_DECL, locator, '');
    Exit;
  end;

  if Assigned(NextHandler) then begin
    XmlParameterEntityDeclarationSignal := TXmlParameterEntityDeclarationSignal.CreateFromLocator(Self, Locator);
    try
      XmlParameterEntityDeclarationSignal.EntityName := entityName;
      XmlParameterEntityDeclarationSignal.EntityValue := entityValue;
      XmlParameterEntityDeclarationSignal.PublicId := PubidLiteral;
      XmlParameterEntityDeclarationSignal.SystemId := SystemLiteral;
      NextHandler.processSignal(XmlParameterEntityDeclarationSignal);
    finally
      XmlParameterEntityDeclarationSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDtdReader.WriteStartExtDtd(const locator: IDomLocator;
                                                 const inputEnc,
                                                       pubId,
                                                       sysId,
                                                       version,
                                                       encName: wideString);
var
  XmlStartExtDtdSignal: TXmlStartExtDtdSignal;
begin
  if Assigned(NextHandler) then begin
    XmlStartExtDtdSignal := TXmlStartExtDtdSignal.CreateFromLocator(Self, Locator);
    try
      XmlStartExtDtdSignal.InputEncoding := inputEnc;
      XmlStartExtDtdSignal.PublicId := pubId;
      XmlStartExtDtdSignal.SystemId := sysId;
      XmlStartExtDtdSignal.Version := version;
      XmlStartExtDtdSignal.EncodingName := encName;
      NextHandler.processSignal(XmlStartExtDtdSignal);
    finally
      XmlStartExtDtdSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDtdReader.WriteStartIntDtd(const locator: IDomLocator;
                                                 const sysId: wideString);
var
  XmlStartIntDtdSignal: TXmlStartIntDtdSignal;
begin
  if Assigned(NextHandler) then begin
    XmlStartIntDtdSignal := TXmlStartIntDtdSignal.CreateFromLocator(Self, Locator);
    try
      XmlStartIntDtdSignal.SystemId := sysId;
      NextHandler.processSignal(XmlStartIntDtdSignal);
    finally
      XmlStartIntDtdSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDtdReader.parseloop(const Tokenizer: TXmlCustomSubsetTokenizer);
begin
  with Tokenizer do begin
    while not (tokenType = DTD_ABSTRACT_END_OF_SOURCE_TOKEN) do begin
      Next;

      if errorType <> ET_NONE then
        SendErrorNotification(errorType, Tokenizer, tokenName + ' ' + tokenData);

      case Tokenizer.tokenType of
        DTD_ABSTRACT_ATTLIST_DECL_TOKEN:
          WriteAttributeDeclaration(Tokenizer, tokenName, tokenData);

        DTD_ABSTRACT_COMMENT_TOKEN:
          WriteDTDComment(Tokenizer, tokenData);

        DTD_ABSTRACT_ELEMENT_DECL_TOKEN:
          WriteElementDeclaration(Tokenizer, tokenName, tokenData);

        DTD_ABSTRACT_ENTITY_DECL_TOKEN:
          WriteEntityDeclaration(Tokenizer, tokenName, tokenData);

        DTD_ABSTRACT_NOTATION_DECL_TOKEN:
          WriteNotationDeclaration(Tokenizer, tokenName, tokenData);

        DTD_ABSTRACT_PARAMETER_ENTITY_DECL_TOKEN:
          WriteParameterEntityDeclaration(Tokenizer, tokenName, tokenData);

        DTD_ABSTRACT_PARAMETER_ENTITY_REF_TOKEN:
          if PERefTreatment = PT_STOP then
            Exit;  // Remark: The Tokenizer returns an DTD_ABSTRACT_PARAMETER_ENTITY_REF_TOKEN
                   //         only if PERefTreatment is 'PT_STOP' or 'PT_SKIP',
                   //         or if the parameter entity reference was not
                   //         wellformed. (The latter case was already been
                   //         handled above.)
        DTD_ABSTRACT_PI_TOKEN:
          WriteDTDProcessingInstruction(Tokenizer, tokenName, tokenData);
      end;

    end;
  end;
end;

function TXmlStandardDtdReader.parseExternalSubset(const inputSource: TXmlInputSource): boolean;
var
  Tokenizer: TXmlExtSubsetTokenizer;
begin
  Tokenizer := TXmlExtSubsetTokenizer.Create(InputSource, TabWidth, FPERepository);
  try
    try
      with InputSource do
        writeStartExtDtd(Tokenizer, inputEncoding, publicId, systemId, xmlVersion, xmlEncoding);
      with Tokenizer do
        if errorType = ET_NONE
          then parseloop(Tokenizer)
          else sendErrorNotification(errorType, Tokenizer, tokenName + ' ' + tokenData);
      writeCompleted(Tokenizer);
    except
      SendAbortedSignal(Tokenizer);
      FXmlErrorDetected := True;
    end;
  finally
    Tokenizer.Free;
  end;
  Result := not FXmlErrorDetected;
end;

function TXmlStandardDtdReader.parseInternalSubset(const inputSource: TXmlInputSource): boolean;
var
  Tokenizer: TXmlIntSubsetTokenizer;
begin
  Tokenizer := TXmlIntSubsetTokenizer.create(InputSource, TabWidth, FPERepository);
  try
    try
      writeStartIntDtd(Tokenizer, inputSource.systemId);
      with Tokenizer do begin
        resolvePEs := PERefTreatment = PT_PARSE;
        if errorType = ET_NONE
          then parseloop(Tokenizer)
          else sendErrorNotification(errorType, Tokenizer, tokenName + ' ' + tokenData);
      end;
      writeCompleted(Tokenizer);
    except
      SendAbortedSignal(Tokenizer);
      FXmlErrorDetected := True;
    end;
  finally
    Tokenizer.Free;
  end;
  Result := not FXmlErrorDetected;
end;

procedure TXmlStandardDtdReader.prepare;
begin
  FAttrListDeclNames.Clear;
  FElementTypeDeclNames.Clear;
  FPERepository.Clear;
  FXmlErrorDetected := False;
end;



// +++++++++++++++++++++++ TXmlStandardDomReader +++++++++++++++++++++++
constructor TXmlStandardDomReader.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  FIgnoreUnspecified:= true;
  FSuppressXmlns:= false;
  FPrefixMapping:= true;
end;

function TXmlStandardDomReader.getContextNode: TdomNode;
begin
  result := FContextNode;
end;

function TXmlStandardDomReader.getSystemId: wideString;
begin
  if assigned(contextNode) then begin
    if assigned(contextNode.referenceDocument)
      then result := contextNode.referenceDocument.documentUri
      else result := '';
  end else result := '';
end;

procedure TXmlStandardDomReader.parseloop(const sourceNode: TdomNode);
var
  i: integer;
  attributeList: TUtilsNameValueList;
  contextNodeBackup: TdomNode;
begin
  contextNodeBackup:= FContextNode;
  FContextNode:= sourceNode;
  try
    case sourceNode.nodeType of
      ntElement_Node: begin
        attributeList:= TUtilsNameValueList.create;
        try
          with sourceNode.attributes do begin
            if FPrefixMapping then begin
              if FSuppressXmlns then begin
                for i:= 0 to pred(length) do begin
                  with (item(i) as TdomAttr) do begin
                    case isXmlnsDecl of
                      NSDT_NONE: attributeList.add(nodeName,nodeValue);
                      NSDT_DEFAULT: writeStartPrefixMapping('',nodeValue);
                      NSDT_PREFIXED: writeStartPrefixMapping(xmlExtractLocalName(nodeName),nodeValue);
                    end;
                  end;
                end; {for ...}
              end else begin
                for i:= 0 to pred(length) do begin
                  with (item(i) as TdomAttr) do begin
                    case isXmlnsDecl of
                      NSDT_DEFAULT: writeStartPrefixMapping('',nodeValue);
                      NSDT_PREFIXED: writeStartPrefixMapping(xmlExtractLocalName(nodeName),nodeValue);
                    end;
                    attributeList.add(nodeName,nodeValue);
                  end;
                end; {for ...}
              end;
            end else begin
              if FSuppressXmlns then begin
                if FIgnoreUnspecified then begin
                  for i:= 0 to pred(length) do
                    with (item(i) as TdomAttr) do
                      if specified and (isXmlnsDecl = NSDT_NONE) then
                        attributeList.add(nodeName,nodeValue);
                end else begin
                  for i:= 0 to pred(length) do
                    with (item(i) as TdomAttr) do
                      if isXmlnsDecl = NSDT_NONE then
                        attributeList.add(nodeName,nodeValue);
                end;
              end else begin
                if FIgnoreUnspecified then begin
                  for i:= 0 to pred(length) do
                    with (item(i) as TdomAttr) do
                      if specified then
                        attributeList.add(nodeName,nodeValue);
                end else begin
                  attributeList.capacity:= attributeList.capacity + length;
                  for i:= 0 to pred(length) do
                    with (item(i) as TdomAttr) do
                      attributeList.add(nodeName,nodeValue);
                end;
              end; {if ... else ...}
            end; {if ...}
          end; {with ...}

          with sourceNode do begin
            writeStartElement(nodeName,attributeList);
            with childNodes do
              for i:= 0  to pred(length) do
                parseloop(item(i));
            writeEndElement(nodeName);
          end; {with...}

          if FPrefixMapping then
            with sourceNode.attributes do
              for i:= pred(length) downto 0 do // Remark: Iterating again is faster than storing the wideSrings in the first pass above
                with (item(i) as TdomAttr) do
                  case isXmlnsDecl of
                    NSDT_DEFAULT: writeEndPrefixMapping('');
                    NSDT_PREFIXED: writeEndPrefixMapping(xmlExtractLocalName(nodeName));
                  end;

        finally
          attributeList.free;
        end;
      end;
      ntText_Node:
        WritePCData(sourceNode.nodeValue);
      ntCDATA_Section_Node:
        WriteCDATA(sourceNode.nodeValue);
      ntEntity_Reference_Node:
        WriteEntityRef(sourceNode.nodeName);
      ntProcessing_Instruction_Node:
        WriteProcessingInstruction(sourceNode.nodeName,sourceNode.nodeValue);
      ntComment_Node:
        WriteComment(sourceNode.nodeValue);
      ntDocument_Node:
        for i:= 0  to pred(sourceNode.childNodes.length) do
          parseloop(sourceNode.childNodes.item(i));
      ntDocument_Type_Decl_Node:
        WriteDoctype(sourceNode.nodeName,
                     (sourceNode as TdomDocumentTypeDecl).publicId,
                     (sourceNode as TdomDocumentTypeDecl).systemId,
                     (sourceNode as TdomDocumentTypeDecl).internalSubset);
      else
        raise EParserException.create('Internal Parser error.');
    end;
  finally
    FContextNode:= contextNodeBackup;
  end;
end;

procedure TXmlStandardDomReader.SendAbortedSignal;
var
  XmlAbortedSignal: TXmlAbortedSignal;
begin
  if Assigned(NextHandler) then begin
    XmlAbortedSignal := TXmlAbortedSignal.Create(Self, 0, 0, 0, 1, 0, 0, 0, 1, '',nil,nil);  // xxx Addd more valuable information!
    try
      NextHandler.ProcessSignal(XmlAbortedSignal);
    finally
      XmlAbortedSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDomReader.WriteCDATA(const content: wideString);
var
  XmlCDATASignal: TXmlCDATASignal;
begin
  if Assigned(NextHandler) then begin
    XmlCDATASignal := TXmlCDATASignal.Create(Self, 0, 0, 0, 1, 0, 0, 0, 1, getSystemId, nil, contextNode);
    try
      XmlCDATASignal.Data := content;
      NextHandler.processSignal(XmlCDATASignal);
    finally
      XmlCDATASignal.Free;
    end;
  end;
end;

procedure TXmlStandardDomReader.WriteComment(const content: wideString);
var
  XmlCommentSignal: TXmlCommentSignal;
begin
  if Assigned(NextHandler) then begin
    XmlCommentSignal := TXmlCommentSignal.Create(Self, 0, 0, 0, 1, 0, 0, 0, 1, getSystemId, nil, contextNode);
    try
      XmlCommentSignal.Data := content;
      NextHandler.processSignal(XmlCommentSignal);
    finally
      XmlCommentSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDomReader.WriteDoctype(const doctypeName,
                                                   publicId,
                                                   systemId,
                                                   intSubset: wideString);
var
  XmlDoctypeSignal: TXmlDoctypeSignal;
begin
  if Assigned(NextHandler) then begin
    XmlDoctypeSignal := TXmlDoctypeSignal.Create(Self, 0, 0, 0, 1, 0, 0, 0, 1, getSystemId, nil, contextNode);
    try
      XmlDoctypeSignal.DoctypeName := doctypeName;
      XmlDoctypeSignal.PublicId := publicId;
      XmlDoctypeSignal.SystemId := systemId;
      XmlDoctypeSignal.Data := intSubset;
      NextHandler.processSignal(XmlDoctypeSignal);
    finally
      XmlDoctypeSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDomReader.WriteEndElement(const tagName: wideString);
var
  XmlEndElementSignal: TXmlEndElementSignal;
begin
  if Assigned(NextHandler) then begin
    XmlEndElementSignal := TXmlEndElementSignal.Create(Self, 0, 0, 0, 1, 0, 0, 0, 1, getSystemId, nil, contextNode);
    try
      XmlEndElementSignal.TagName := TagName;
      NextHandler.processSignal(XmlEndElementSignal);
    finally
      XmlEndElementSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDomReader.WriteEndPrefixMapping(const prefix: wideString);
var
  XmlEndPrefixMappingSignal: TXmlEndPrefixMappingSignal;
begin
  if Assigned(NextHandler) then begin
    XmlEndPrefixMappingSignal := TXmlEndPrefixMappingSignal.Create(Self, 0, 0, 0, 1, 0, 0, 0, 1, getSystemId, nil, contextNode);
    try
      XmlEndPrefixMappingSignal.Prefix := Prefix;
      NextHandler.processSignal(XmlEndPrefixMappingSignal);
    finally
      XmlEndPrefixMappingSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDomReader.WriteEntityRef(const entityName: wideString);
var
  XmlEntityRefSignal: TXmlEntityRefSignal;
begin
  if Assigned(NextHandler) then begin
    XmlEntityRefSignal := TXmlEntityRefSignal.Create(Self, 0, 0, 0, 1, 0, 0, 0, 1, getSystemId, nil, contextNode);
    try
      XmlEntityRefSignal.EntityName := EntityName;
      NextHandler.processSignal(XmlEntityRefSignal);
    finally
      XmlEntityRefSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDomReader.WriteCompleted;
var
  XmlCompletedSignal: TXmlCompletedSignal;
begin
  if Assigned(NextHandler) then begin
    XmlCompletedSignal := TXmlCompletedSignal.Create(Self, 0, 0, 0, 1, 0, 0, 0, 1, getSystemId, nil, contextNode);
    try
      NextHandler.ProcessSignal(XmlCompletedSignal);
    finally
      XmlCompletedSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDomReader.WritePCDATA(const content: wideString);
var
  XmlPCDATASignal: TXmlPCDATASignal;
begin
  if content = '' then Exit;

  if Assigned(NextHandler) then begin
    XmlPCDATASignal := TXmlPCDATASignal.Create(Self, 0, 0, 0, 1, 0, 0, 0, 1, getSystemId, nil, contextNode);
    try
      XmlPCDATASignal.Data := content;
      NextHandler.processSignal(XmlPCDATASignal);
    finally
      XmlPCDATASignal.Free;
    end;
  end;
end;

procedure TXmlStandardDomReader.WriteProcessingInstruction(const targ,
                                                                 attribSequence : wideString);
var
  XmlProcessingInstructionSignal: TXmlProcessingInstructionSignal;
begin
  if Assigned(NextHandler) then begin
    XmlProcessingInstructionSignal := TXmlProcessingInstructionSignal.Create(Self, 0, 0, 0, 1, 0, 0, 0, 1, getSystemId, nil, contextNode);
    try
      XmlProcessingInstructionSignal.Target := Targ;
      XmlProcessingInstructionSignal.Data := AttribSequence;
      NextHandler.processSignal(XmlProcessingInstructionSignal);
    finally
      XmlProcessingInstructionSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDomReader.WriteStartDocument(const inputEnc,
                                                         version,
                                                         encName: wideString;
                                                         sdDl: TdomStandalone);
var
  XmlStartDocumentSignal: TXmlStartDocumentSignal;
begin
  if Assigned(NextHandler) then begin
    XmlStartDocumentSignal := TXmlStartDocumentSignal.Create(Self, 0, 0, 0, 1, 0, 0, 0, 1, getSystemId, nil, contextNode);
    try
      XmlStartDocumentSignal.InputEncoding  := InputEnc;
      XmlStartDocumentSignal.Version  := Version;
      XmlStartDocumentSignal.EncodingName   := EncName;
      XmlStartDocumentSignal.StandaloneDecl := SdDl;
      NextHandler.processSignal(XmlStartDocumentSignal);
    finally
      XmlStartDocumentSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDomReader.WriteStartDocumentFragment(const encName: wideString);
var
  XmlStartDocumentFragmentSignal: TXmlStartDocumentFragmentSignal;
begin
  if Assigned(NextHandler) then begin
    XmlStartDocumentFragmentSignal := TXmlStartDocumentFragmentSignal.Create(Self, 0, 0, 0, 1, 0, 0, 0, 1, getSystemId, nil, contextNode);
    try
      XmlStartDocumentFragmentSignal.EncodingName  := encName;
      NextHandler.processSignal(XmlStartDocumentFragmentSignal);
    finally
      XmlStartDocumentFragmentSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDomReader.WriteStartElement(const tagName: wideString;
                                                  const attributeList: TUtilsNameValueList);
var
  XmlStartElementSignal: TXmlStartElementSignal;
begin
  if Assigned(NextHandler) then begin
    XmlStartElementSignal := TXmlStartElementSignal.Create(Self, 0, 0, 0, 1, 0, 0, 0, 1, getSystemId, nil, contextNode);
    try
      XmlStartElementSignal.TagName    := TagName;
      XmlStartElementSignal.Attributes := AttributeList;  
      NextHandler.processSignal(XmlStartElementSignal);
    finally
      XmlStartElementSignal.Free;
    end;
  end;
end;

procedure TXmlStandardDomReader.WriteStartPrefixMapping(const prefix,
                                                             uri: wideString);
var
  XmlStartPrefixMappingSignal: TXmlStartPrefixMappingSignal;
begin
  if Assigned(NextHandler) then begin
    XmlStartPrefixMappingSignal := TXmlStartPrefixMappingSignal.Create(Self, 0, 0, 0, 1, 0, 0, 0, 1, getSystemId, nil, contextNode);
    try
      XmlStartPrefixMappingSignal.Prefix    := Prefix;
      XmlStartPrefixMappingSignal.Uri := Uri;
      NextHandler.processSignal(XmlStartPrefixMappingSignal);
    finally
      XmlStartPrefixMappingSignal.Free;
    end;
  end;
end;

function TXmlStandardDomReader.parse(const sourceNode: TdomNode): boolean;
begin
  Result := True;
  try

    if sourceNode.nodeType = ntDocument_Node then begin
      FContextNode:= sourceNode;
      with (sourceNode as TdomDocument) do
        writeStartDocument(inputEncoding, xmlVersion, xmlEncoding, xmlStandalone);
      writePCDATA(#10); // Insert LF after XML declaration.
    end else begin
      FContextNode:= nil;
      with sourceNode.referenceDocument do
        if xmlEncoding = ''
          then writeStartDocumentFragment(inputEncoding)
          else writeStartDocumentFragment(xmlEncoding);
    end;

    parseloop(sourceNode);

    writeCompleted;

  except
    SendAbortedSignal;
    Result := False;
  end; {try ...}
end;



// +++++++++++++++++++++++++ TXmlCustomParser +++++++++++++++++++++++++
constructor TXmlCustomParser.create(aOwner: TComponent);
begin
  inherited;
  FDOMImpl:= nil;
end;

procedure TXmlCustomParser.setDomImpl(const impl: TDomImplementation);
begin
  if FDOMImpl = impl then exit;
  {$IFDEF VER140+}
  if assigned(FDOMImpl)
    then FDOMImpl.RemoveFreeNotification(Self);
  {$ENDIF}
  {$IFDEF LINUX}
  if assigned(FDOMImpl)
    then FDOMImpl.RemoveFreeNotification(Self);
  {$ENDIF}
  FDOMImpl:= impl;
  if assigned(impl)
    then impl.FreeNotification(Self);
end;

procedure TXmlCustomParser.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent,Operation);
  if (Operation = opRemove) and (AComponent = FDomImpl)
    then FDomImpl:= nil;
end;



{ TXmlToDomParser }

{constructor / destructor}

constructor TXmlToDomParser.create(aOwner: TComponent);
begin
  inherited;
  createSubcomponents;
  FBufferSize:= 4096;
  FNewDoc:= nil;
  AutoPrepare:= AP_NO;
  KeepCDATASections:= true;
  KeepComments:= true;
  KeepEntityRefs:= true;
end;

destructor TXmlToDomParser.destroy;
begin
  if assigned(FNewDoc) then
    try
      FNewDoc.domImplementation.freeDocument(FNewDoc);
    except
      // do nothing.
    end;
  inherited;
end;

procedure TXmlToDomParser.createSubcomponents;
begin
  FDocReader:= TXmlStandardDocReader.create(self);
  FWFTestHandler:= TXmlWFTestHandler.create(self);
  FDocBuilder:= TXmlDomBuilder.create(self);

  FDocReader.DOMImpl:= FDOMImpl;
  FDocReader.PrefixMapping:= false;
  FDocReader.SuppressXmlns:= false;

  FDocReader.NextHandler:= FWFTestHandler;
  FWFTestHandler.NextHandler:= FDocBuilder;

  FDocBuilder.BuildNamespaceTree:= false;
end;

{property methods}

function TXmlToDomParser.getAutoPrepare: TdomAutoPrepare;
begin
  result:= FDocBuilder.AutoPrepare;
end;

function TXmlToDomParser.getKeepCDATASections: boolean;
begin
  result:= FDocBuilder.KeepCDATASections;
end;

function TXmlToDomParser.getKeepComments: boolean;
begin
  result:= FDocBuilder.KeepComments;
end;

function TXmlToDomParser.getKeepEntityRefs: boolean;
begin
  result:= FDocBuilder.KeepEntityRefs;
end;

procedure TXmlToDomParser.setAutoPrepare(const Value: TdomAutoPrepare);
begin
  FDocBuilder.AutoPrepare:= value;
end;

procedure TXmlToDomParser.setBufferSize(const value: integer);
begin
  if value < 1024
    then raise ENot_Supported_Err.create('BufferSize must not be less than 1024.');
  FBufferSize:= value;
end;

procedure TXmlToDomParser.setKeepCDATASections(const value: boolean);
begin
  FDocBuilder.KeepCDATASections:= value;
end;

procedure TXmlToDomParser.setKeepComments(const value: boolean);
begin
  FDocBuilder.KeepComments:= value;
end;

procedure TXmlToDomParser.setKeepEntityRefs(const value: boolean);
begin
  FDocBuilder.KeepEntityRefs:= value;
end;

procedure TXmlToDomParser.setDomImpl(const impl: TDomImplementation);
begin
  inherited;
  FDocReader.DOMImpl:= impl;
end;

function TXmlToDomParser.sendErrorNotification(const xmlErrorType: TXmlErrorType): boolean;
var
  error: TdomError;
begin
  error:= TdomError.createFromLocator(xmlErrorType,nil,'');
  try
    if assigned(FDomImpl)
      then result:= FDomImpl.handleError(self,error)
      else result:= not (error.severity = DOM_SEVERITY_FATAL_ERROR);
  finally
    error.free;
  end;
end;

function TXmlToDomParser.fileToDom(const filename: TFileName): TdomDocument;
var
  SourceStream: TFileStream;
begin
  if Filename = '' then
    raise EAccessViolation.create('Filename not specified.');
  SourceStream:= TFileStream.Create(filename,fmOpenRead);
  try
    Result := parseStream(SourceStream,'',FilenameToUriWideStr(Filename,[]),nil) as TdomDocument;
  finally
    SourceStream.free;
  end; {try}
end;

function TXmlToDomParser.parseSourceCode(const docSourceCode: TXmlSourceCode;
                                         const pubId,
                                               sysId: wideString;
                                         const refNode: TdomNode): TdomNode;
begin
  if not assigned(docSourceCode) then
    raise EAccessViolation.create('Source code not specified.');
  result:= parseWideString(docSourceCode.text,pubId,sysId,refNode);
end;

function TXmlToDomParser.parseStream(const stream: TStream;
                                     const pubId,
                                           sysId: wideString;
                                     const refNode: TdomNode): TdomNode;
var
  InputSrc: TXmlInputSource;
begin
  if not Assigned(Stream) then
    raise EAccessViolation.create('Stream not specified.');
  InputSrc := TXmlInputSource.create(Stream, PubId, SysId, FBufferSize, 'UTF-8',
                False, 0, 0, 0, 0, 1);  // xxx implement default encoding?   xxx Change offsetFromBeginning parameter?
  try
    Result:= parseXmlInputSource(InputSrc, Refnode);
  finally
    InputSrc.free;
  end; {try}
end;

function TXmlToDomParser.parseString(const str: string;
                                     const pubId,
                                           sysId: wideString;
                                     const refNode: TdomNode): TdomNode;
var
  StrStream: TStringStream;
begin
  if str = '' then
    raise EAccessViolation.create('Empty string.');
  StrStream:= TStringStream.create(str);
  try
    result:= parseStream(StrStream,pubId,sysId,refNode);
  finally
    StrStream.free;
  end; {try}
end;

function TXmlToDomParser.parseURI(      pubId,
                                        sysId: wideString;
                                  const refNode: TdomNode): TdomNode;
var
  Stream: TStream;
begin
  Stream := FDOMImpl.ResolveResource('', PubId, SysId); // Creates Stream.
  if Assigned(Stream) then begin
    try
      Result := ParseStream(Stream, PubId, SysId, RefNode);
    finally
      Stream.Free;
    end;
  end else Result := nil;
end;

function TXmlToDomParser.parseWideString(const str: wideString;
                                         const pubId,
                                               sysId: wideString;
                                         const refNode: TdomNode): TdomNode;
var
  WStrStream: TUtilsWideStringStream;
  S: wideString;
begin
  if str = '' then raise EAccessViolation.create('Empty string.');
  if str[1] = #$FEFF then begin  // Byte Order Mark?
    WStrStream:= TUtilsWideStringStream.create(str);
  end else begin
    S := concat(wideString(#$FEFF),str);
    WStrStream:= TUtilsWideStringStream.create(S);
  end;
  try
    result:= parseStream(WStrStream,pubId,sysId,refNode);
  finally
    WStrStream.free;
  end; {try}
end;

function TXmlToDomParser.parseXmlInputSource(const inputSource: TXmlInputSource;
                                             const refNode: TdomNode): TdomNode;
var
  newDoc: TdomDocument;
begin
  if not Assigned(inputSource) then
    raise EAccessViolation.create('Input source not specified.');
  if not Assigned(FDOMImpl) then
    raise EAccessViolation.Create('DOM implementation not specified.');
  if Assigned(RefNode) then
    if FDOMImpl <> RefNode.OwnerDocument.DomImplementation then
      raise EWrong_DOM_Implementation_Err.Create('Wrong DOM implementation error.');

  if inputSource.hasMalformedDecl
   or not ( inputSource.declType in [ DT_XML_DECLARATION,
                                      DT_XML_OR_TEXT_DECLARATION,
                                      DT_UNSPECIFIED ] )
  then begin
    sendErrorNotification(ET_INVALID_XML_DECL);
    raise EParserException.create('Parser error.');
  end;
  if inputSource.invalidEncoding then begin
    sendErrorNotification(ET_ENCODING_NOT_SUPPORTED);
    raise EParserException.create('Parser error.');
  end;

  if assigned(refNode) then begin
    FDocBuilder.referenceNode:= refNode;
    if not FDocReader.parseFragment(inputSource) then
      raise EParserException.create('Parser error.');
    Result:= refNode;
  end else begin
    newDoc:= FDOMImpl.createDoc;
    try
      newDoc.documentUri := inputSource.systemId;
      FDocBuilder.referenceNode:= newDoc;
      if not FDocReader.parse(inputSource) then
        raise EParserException.create('Parser error.');
      result:= newDoc;
    except
      FDOMImpl.freeDocument(newDoc);
      raise;
    end;
  end;
end;

procedure TXmlIntSubsetTokenizer.setResolvePEs(const value: boolean);
begin
  FResolvePEs := Value;
end;



{ TDtdToASParser }

constructor TDtdToASParser.create(aOwner: TComponent);
begin
  inherited;
  createSubcomponents;
  FBufferSize:= 4096;
  PERefTreatment := PT_PARSE;
end;

procedure TDtdToASParser.createSubcomponents;
begin
  FDtdReader:= TXmlStandardDtdReader.create(self);
  FDtdReader.DOMImpl:= FDOMImpl;

  FWFTestHandler:= TXmlWFTestHandler.create(self);
  FASBuilder:= TXmlASBuilder.create(self);

  FDtdReader.NextHandler:= FWFTestHandler;
  FWFTestHandler.NextHandler:= FASBuilder;
end;

procedure TDtdToASParser.extDtdSourceCodeToAS(const ExtDtdSourceCode: TXmlSourceCode;
                                              const pubId,
                                                    sysId: wideString;
                                              const target: TdomASModel);
begin
  if not assigned(extDtdSourceCode) then raise EAccessViolation.create('Source code not specified.');
  extDtdWideStringToAS(extDtdSourceCode.text, pubId, sysId, target);
end;

procedure TDtdToASParser.extDtdStreamToAS(const stream: TStream;
                                          const pubId,
                                                sysId: wideString;
                                          const target: TdomASModel);
var
  InputSrc: TXmlInputSource;
begin
  if not assigned(stream) then raise EAccessViolation.create('Stream not specified.');
  FASBuilder.ASModel:= target;
  InputSrc := TXmlInputSource.create(Stream, PubId, SysId, FBufferSize, 'UTF-8',
                False, 0, 0, 0, 0, 1);  // xxx implement default encoding?   xxx Change offsetFromBeginning parameter?
  try
    if InputSrc.hasMalformedDecl
     or not ( InputSrc.declType in [ DT_TEXT_DECLARATION,
                                     DT_XML_OR_TEXT_DECLARATION,
                                     DT_UNSPECIFIED] )
    then begin
      sendErrorNotification(ET_INVALID_TEXT_DECL);
      raise EParserException.create('Parser error.');
    end else begin
      if InputSrc.invalidEncoding then begin
        sendErrorNotification(ET_ENCODING_NOT_SUPPORTED);
        raise EParserException.create('Parser error.');
      end else begin
        if not FDtdReader.parseExternalSubset(InputSrc) then
          raise EParserException.create('Parser error.');
      end;
    end;
  finally
    InputSrc.free;
  end; {try}
end;

procedure TDtdToASParser.extDtdStringToAS(const str: string;
                                          const pubId,
                                                sysId: wideString;
                                          const target: TdomASModel);
var
  StrStream: TStringStream;
begin
  if str = '' then raise EAccessViolation.create('Empty string.');
  StrStream:= TStringStream.create(str);
  try
    extDtdStreamToAS(StrStream, pubId, sysId, target);
  finally
    StrStream.free;
  end; {try}
end;

procedure TDtdToASParser.extDtdWideStringToAS(      str: wideString;
                                              const pubId,
                                                    sysId: wideString;
                                              const target: TdomASModel);
var
  WStrStream: TUtilsWideStringStream;
begin
  if str = '' then raise EAccessViolation.create('Empty string.');
  if str[1] <> #$FEFF then
    str:= concat(wideString(#$FEFF),str);
  WStrStream:= TUtilsWideStringStream.create(str);
  try
    extDtdStreamToAS(WStrStream, pubId, sysId,  target);
  finally
    WStrStream.free;
  end; {try}
end;

function TDtdToASParser.getPERefTreatment: TdomPERefTreatment;
begin
  Result := FDtdReader.PERefTreatment;
end;

procedure TDtdToASParser.intDtdSourceCodeToAS(const intDtdSourceCode: TXmlSourceCode;
                                              const pubId,
                                                    sysId: wideString;
                                              const intSubsetStartByteNumber,
                                                    intSubsetStartCharNumber,
                                                    intSubsetStartColumn,
                                                    intSubsetStartLine: Int64;
                                              const target: TdomASModel);
begin
  if not assigned(intDtdSourceCode) then raise EAccessViolation.create('Source code not specified.');
  intDtdWideStringToAS(intDtdSourceCode.text, pubId, sysId,
    intSubsetStartByteNumber, intSubsetStartCharNumber, intSubsetStartColumn,
    intSubsetStartLine, target);
end;

procedure TDtdToASParser.intDtdStreamToAS(const stream: TStream;
                                          const pubId,
                                                sysId: wideString;
                                          const intSubsetStartByteNumber,
                                                intSubsetStartCharNumber,
                                                intSubsetStartColumn,
                                                intSubsetStartLine: Int64;
                                          const target: TdomASModel);
var
  InputSrc: TXmlInputSource;
begin
  if not assigned(stream) then raise EAccessViolation.create('Stream not specified.');
  FASBuilder.ASModel:= target;
  InputSrc := TXmlInputSource.create(stream, pubId, sysId, FBufferSize, 'UTF-8',
                False, intSubsetStartByteNumber, intSubsetStartCharNumber,
                intSubsetStartColumn, 0, intSubsetStartLine);  // xxx implement default encoding and ByteOffset?   xxx Change offsetFromBeginning parameter?
  try
    if InputSrc.hasMalformedDecl
     or not ( InputSrc.declType in [ DT_TEXT_DECLARATION,
                                     DT_XML_OR_TEXT_DECLARATION,
                                     DT_UNSPECIFIED] )
    then begin
      sendErrorNotification(ET_INVALID_TEXT_DECL);
      raise EParserException.create('Parser error.');
    end else begin
      if InputSrc.invalidEncoding then begin
        sendErrorNotification(ET_ENCODING_NOT_SUPPORTED);
        raise EParserException.create('Parser error.');
      end else begin
        if not FDtdReader.parseInternalSubset(InputSrc) then
          raise EParserException.create('Parser error.');
      end;
    end;
  finally
    InputSrc.free;
  end; {try}
end;

procedure TDtdToASParser.intDtdStringToAS(const str: string;
                                          const pubId,
                                                sysId: wideString;
                                          const intSubsetStartByteNumber,
                                                intSubsetStartCharNumber,
                                                intSubsetStartColumn,
                                                intSubsetStartLine: Int64;
                                          const target: TdomASModel);
var
  StrStream: TStringStream;
begin
  if str = '' then raise EAccessViolation.create('Empty string.');
  StrStream:= TStringStream.create(str);
  try
    intDtdStreamToAS(StrStream, pubId, sysId, intSubsetStartByteNumber,
      intSubsetStartCharNumber, intSubsetStartColumn, intSubsetStartLine,
      target);
  finally
    StrStream.free;
  end; {try}
end;

procedure TDtdToASParser.intDtdWideStringToAS(      str: wideString;
                                              const pubId,
                                                    sysId: wideString;
                                              const intSubsetStartByteNumber,
                                                    intSubsetStartCharNumber,
                                                    intSubsetStartColumn,
                                                    intSubsetStartLine: Int64;
                                              const target: TdomASModel);
var
  WStrStream: TUtilsWideStringStream;
begin
  if Str = '' then raise EAccessViolation.Create('Empty string.');
  if Str[1] <> #$FEFF then
    Str := concat(WideString(#$FEFF), Str);
  WStrStream:= TUtilsWideStringStream.Create(Str);
  try
    IntDtdStreamToAS(WStrStream, PubId, SysId, intSubsetStartByteNumber,
      intSubsetStartCharNumber, intSubsetStartColumn, intSubsetStartLine,
      target);
  finally
    WStrStream.Free;
  end; {try}
end;

function TDtdToASParser.parseDTD(const intSubset,
                                       docUri: wideString;
                                 const intSubsetStartByteNumber,
                                       intSubsetStartCharNumber,
                                       intSubsetStartColumn,
                                       intSubsetStartLine: Int64;
                                       pubId,
                                       sysId: wideString;
                                 const ASModelCollection: TdomASModelCollection): boolean;
var
  extDtdStream: TStream;
  sysUri: wideString;
begin
  result := true;
  prepare;
  if intSubset <> '' then begin
    with DOMImpl do begin
      try
        intDtdWideStringToAS(intSubset, '', docUri, intSubsetStartByteNumber,
          intSubsetStartCharNumber, intSubsetStartColumn, intSubsetStartLine,
          ASModelCollection.internalSubset);
      except
        result:= false;
      end;
    end;
  end;

  if result and ( (pubId <> '') or (sysId <> '') ) then begin
    extDtdStream:= nil;
    if assigned(DOMImpl) then
      extDtdStream:= DOMImpl.resolveResource(docUri, pubId, sysId);
    if assigned(extDtdStream) then begin
      try
        with DOMImpl do begin
          ResolveRelativeUriWideStr(docUri, sysId, sysUri);
          try
            extDtdStreamToAS(extDtdStream, pubId, sysUri, ASModelCollection.externalSubset);
          except
            result:= false;
          end;
        end; {with ...}
      finally
        extDtdStream.free;
      end; {try ... finally ...}
    end else begin
      sendErrorNotification(ET_EXTERNAL_SUBSET_NOT_FOUND);
      result:= false;
    end; {if ...}
  end; {if ...}
end;

procedure TDtdToASParser.prepare;
begin
  FDtdReader.prepare;
end;

function TDtdToASParser.sendErrorNotification(const xmlErrorType: TXmlErrorType): boolean;
var
  error: TdomError;
begin
  error:= TdomError.createFromLocator(xmlErrorType,nil,'');
  try
    if assigned(FDomImpl) then begin
      result:= FDomImpl.handleError(self,error);
    end else if error.severity = DOM_SEVERITY_FATAL_ERROR
      then result:= false
      else result:= true;
  finally
    error.free;
  end;
end;

procedure TDtdToASParser.setBufferSize(const value: integer);
begin
  if value < 1024
    then raise ENot_Supported_Err.create('BufferSize must not be less than 1024.');
  FBufferSize:= value;
end;

procedure TDtdToASParser.setDomImpl(const impl: TDomImplementation);
begin
  inherited;
  FDtdReader.DOMImpl:= impl;
end;

procedure TDtdToASParser.setPERefTreatment(const Value: TdomPERefTreatment);
begin
  FDtdReader.PERefTreatment := value;
end;



{ TDomToXmlParser }

constructor TDomToXmlParser.create(aOwner: TComponent);
begin
  inherited;

  FDomReader:= TXmlStandardDomReader.create(self);
  FStreamBuilder:= TXmlStreamBuilder.create(self);
  FStreamBuilder.IncludeXmlDecl := true;
  FWFTestHandler:= TXmlWFTestHandler.create(self);
  FWFTestHandler.NextHandler:= FStreamBuilder;
  FDomReader.NextHandler:= FStreamBuilder;
  FDomReader.IgnoreUnspecified:= true;
  FDomReader.PrefixMapping:= false;
  FDomReader.SuppressXmlns:= false;

  FBufferSize:= 4096;
  FUseActiveCodePage:= false;
  FWriteLFOption:= lwCRLF;
end;

function TDomToXmlParser.getIgnoreUnspecified: boolean;
begin
  Result := FDomReader.IgnoreUnspecified;
end;

function TDomToXmlParser.getIncludeXmlDecl: boolean;
begin
  Result := FStreamBuilder.IncludeXmlDecl;
end;

function TDomToXmlParser.getOnAfterWrite: TdomSerializationEvent;
begin
  Result := FStreamBuilder.OnAfterWrite;
end;

function TDomToXmlParser.getOnBeforeWrite: TdomSerializationEvent;
begin
  Result := FStreamBuilder.OnBeforeWrite;
end;

function TDomToXmlParser.getStrictErrorChecking: boolean;
begin
  Result := (FDomReader.NextHandler = FWFTestHandler);
end;

function TDomToXmlParser.getUseByteOrderMark: boolean;
begin
  Result := FStreamBuilder.UseByteOrderMark;
end;

procedure TDomToXmlParser.setBufferSize(const value: integer);
begin
  if value < 1024
    then raise ENot_Supported_Err.create('BufferSize must not be less than 1024.');
  FBufferSize:= value;
end;

procedure TDomToXmlParser.setIgnoreUnspecified(const value: boolean);
begin
   FDomReader.IgnoreUnspecified := Value;
end;

procedure TDomToXmlParser.setIncludeXmlDecl(const value: boolean);
begin
  FStreamBuilder.IncludeXmlDecl := Value;
end;

procedure TDomToXmlParser.setOnAfterWrite(const Value: TdomSerializationEvent);
begin
  FStreamBuilder.OnAfterWrite := Value;
end;

procedure TDomToXmlParser.setOnBeforeWrite(const Value: TdomSerializationEvent);
begin
  FStreamBuilder.OnBeforeWrite := Value;
end;

procedure TDomToXmlParser.setStrictErrorChecking(const value: boolean);
begin
  if value
    then FDomReader.NextHandler:= FWFTestHandler
    else FDomReader.NextHandler:= FStreamBuilder;
end;

{$IFNDEF LINUX}
procedure TDomToXmlParser.setUseActiveCodePage(const value: boolean);
begin
  FUseActiveCodePage:= value;
end;
{$ENDIF}

procedure TDomToXmlParser.setUseByteOrderMark(const value: boolean);
begin
  FStreamBuilder.UseByteOrderMark:= value;
end;

procedure TDomToXmlParser.setWriteLFOption(const value: TCodecWriteLFOption);
begin
  FWriteLFOption:= value;
end;

function TDomToXmlParser.writeToStream(const wnode: TdomNode;
                                       const encoding: wideString; // xxx use xmlEncoding property of OwnerDocument instead?
                                       const destination: TStream): boolean;
var
  outputSource: TXmlOutputSource;
begin
  if not assigned(FDOMImpl)
    then raise EAccessViolation.create('DOMImplementation not specified.');
  if not assigned(destination)
    then raise EAccessViolation.create('Destination stream not specified.');
  if not assigned(wnode)
    then raise EAccessViolation.create('Source node not specified.');

  FDomReader.DOMImpl:= FDOMImpl;
{$IFDEF LINUX}
  FStreamBuilder.defaultEncoding:= encoding;  // Raises an ENot_Supported_Err, if the specified encoding is not supported
{$ELSE}
  if UseActiveCodePage
    then FStreamBuilder.defaultEncoding:= GetSystemEncodingName
    else FStreamBuilder.defaultEncoding:= encoding;  // Raises an ENot_Supported_Err, if the specified encoding is not supported
{$ENDIF}
  outputSource:= TXmlOutputSource.create(destination,FBufferSize);
  try
    outputSource.WriteLFOption:= WriteLFOption;
    FStreamBuilder.outputSource:= outputSource;
    result:= FDomReader.parse(wnode);
  finally
    FStreamBuilder.outputSource:= nil;
    outputSource.free;
  end;
end;

function TDomToXmlParser.writeToString(const wnode: TdomNode;
                                             encoding: wideString;
                                         out S: string): boolean;
var
  xmlStream: TStringStream;
begin
  xmlStream:= TStringStream.create('');
  try
    result:= writeToStream(wnode,encoding,xmlStream);
    S:= xmlStream.dataString;
  finally
    xmlStream.free;
  end;
end;

function TDomToXmlParser.writeToWideString(const wnode: TdomNode;
                                             out S: wideString): boolean;
var
  xmlStream: TUtilsWideStringStream;
begin
  xmlStream:= TUtilsWideStringStream.create('');
  try
    result:= writeToStream(wnode,'UTF-16LE',xmlStream);
    S:= xmlStream.dataString;
  finally
    xmlStream.free;
  end;
end;



{XPath Function Library -- see XPath 1.0, sec. 4}

{XPath Node Set Functions -- see XPath 1.0, sec. 4.1.}

function XPathFunctionLast(const contextNode: TdomNode;
                           const contextPosition: Integer;
                           const contextSize: Integer;
                           const arguments: TList): TdomXPathCustomResult;
begin
  if arguments.Count > 0 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['last']);
  if contextSize < 1 then
    raise EXPath_Invalid_Function_Call_Err.create('Invalid context size.');
  result:= TdomXPathNumberResult.create(contextSize);
end;

function XPathFunctionPosition(const contextNode: TdomNode;
                               const contextPosition: Integer;
                               const contextSize: Integer;
                               const arguments: TList): TdomXPathCustomResult;
begin
  if arguments.Count > 0 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['position']);
  if contextPosition < 1 then
    raise EXPath_Invalid_Function_Call_Err.create('Invalid context position.');
  result:= TdomXPathNumberResult.create(contextPosition);
end;

function XPathFunctionCount(const contextNode: TdomNode;
                            const contextPosition: Integer;
                            const contextSize: Integer;
                            const arguments: TList): TdomXPathCustomResult;
var
  exprResult: TdomXPathCustomResult;
begin
  if arguments.Count <> 1 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['count']);
  exprResult := TdomXPathExpr(arguments[0]).evaluate(contextNode, contextPosition, contextSize);
  try
    if not (exprResult is TdomXPathNodeSetResult) then
      raise EXPath_Invalid_Function_Call_Err.createFmt('Argument mismatch error in %s(): Expression does not evaluate to node-set.',['count']);
    result:= TdomXPathNumberResult.create(TdomXPathNodeSetResult(exprResult).length);
  finally
    exprResult.free;
  end;
end;

function XPathFunctionId(const contextNode: TdomNode;
                         const contextPosition: Integer;
                         const contextSize: Integer;
                         const arguments: TList): TdomXPathCustomResult;
// Note that the returned nodes need not be provided in document order.
// xxx Sort the list? xxx
var
  doc: TdomDocument;
  exprResult: TdomXPathCustomResult;
  i: integer;
  idList: TUtilsWideStringList;
  idNode: TdomNode;

  procedure AddId(const idList: TUtilsWideStringList;
                  const S: WideString);
  const
    NULL: WideChar = #0; // end of wideString mark
  var
    head, tail: PWideChar;
    idString: wideString;
  begin
    // Skip white space:
    head:= PWideChar(S);
    while IsXmlWhiteSpace(head^) do
      inc(head);

    while head^ <> NULL do begin
      // Determine next ID:
      tail:= head;
      while not isXmlWhiteSpaceOrNull(tail^) do
        inc(tail);
      setString(idString, head, tail-head);
      idList.Add(idString);

      // Skip white space:
      head:= tail;
      while IsXmlWhiteSpace(head^) do
        inc(head);
    end;
  end;

begin
  if arguments.Count <> 1 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['id']);
  if not assigned(contextNode) then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Context node not specified for %s().',['id']);
  if not assigned(contextNode.ownerDocument) then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Context node with no owner document specified for %s().',['id']);
  idList := nil; // Remark: This saves one try ... finally block.
  exprResult := TdomXPathExpr(arguments[0]).evaluate(contextNode, contextPosition, contextSize);
  try

    // Determine ID list:
    idList:= TUtilsWideStringList.Create;
    with idList do begin
      Duplicates := dupIgnore;
      Sorted := True;
    end;
    if exprResult is TdomXPathNodeSetResult then begin
      for i := 0 to Pred(TdomXPathNodeSetResult(exprResult).length) do
        AddId(idList, TdomXPathNodeSetResult(exprResult).item(i).XPathStringValue);
    end else AddId(idList, exprResult.asWideString);

    // Find Id nodes:
    result := TdomXPathNodeSetResult.create;
    try
      doc := contextNode.ownerDocument;
      for i := 0 to pred(idList.Count) do begin
        idNode := doc.getElementById(idList[i]);
        if assigned(idNode) then
          TdomXPathNodeSetResult(result).add(idNode);
      end;
    except
      result.Free;
      raise;
    end;

  finally
    idList.free;
    exprResult.free;
  end;
end;

function XPathFunctionLocalName(const contextNode: TdomNode;
                                const contextPosition: Integer;
                                const contextSize: Integer;
                                const arguments: TList): TdomXPathCustomResult;
var
  exprResult: TdomXPathCustomResult;
begin
  if arguments.Count > 1 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['local-name']);
  if arguments.Count = 0 then begin
    exprResult := TdomXPathNodeSetResult.create;
    TdomXPathNodeSetResult(exprResult).add(contextNode);
  end else
    exprResult := TdomXPathExpr(arguments[0]).evaluate(contextNode, contextPosition, contextSize);
  try
    if not (exprResult is TdomXPathNodeSetResult) then
      raise EXPath_Invalid_Function_Call_Err.createFmt('Argument mismatch error in %s(): Expression does not evaluate to node-set.',['local-name']);
    with exprResult do begin
      axisType := XPATH_FORWARD_AXIS;
      if length = 0 then begin
        result:= TdomXPathStringResult.create('');
      end else begin
        if item(0) is TdomProcessingInstruction
          then result:= TdomXPathStringResult.create(TdomProcessingInstruction(item(0)).Target)
          else result:= TdomXPathStringResult.create(TdomNode(item(0)).LocalName);
      end;
    end;
  finally
    exprResult.free;
  end;
end;

function XPathFunctionNamespaceUri(const contextNode: TdomNode;
                                   const contextPosition: Integer;
                                   const contextSize: Integer;
                                   const arguments: TList): TdomXPathCustomResult;
var
  exprResult: TdomXPathCustomResult;
begin
  if arguments.Count > 1 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['namespace-uri']);
  if arguments.Count = 0 then begin
    exprResult := TdomXPathNodeSetResult.create;
    TdomXPathNodeSetResult(exprResult).add(contextNode);
  end else
    exprResult := TdomXPathExpr(arguments[0]).evaluate(contextNode, contextPosition, contextSize);
  try
    if not (exprResult is TdomXPathNodeSetResult) then
      raise EXPath_Invalid_Function_Call_Err.createFmt('Argument mismatch error in %s(): Expression does not evaluate to node-set.',['namespace-uri']);
    with exprResult do begin
      axisType := XPATH_FORWARD_AXIS;
      if length = 0
        then result:= TdomXPathStringResult.create('')
        else result:= TdomXPathStringResult.create(TdomNode(item(0)).NamespaceUri);
    end;
  finally
    exprResult.free;
  end;
end;

function XPathFunctionName(const contextNode: TdomNode;
                           const contextPosition: Integer;
                           const contextSize: Integer;
                           const arguments: TList): TdomXPathCustomResult;
var
  exprResult: TdomXPathCustomResult;
begin
  if arguments.Count > 1 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['name']);
  if arguments.Count = 0 then begin
    exprResult := TdomXPathNodeSetResult.create;
    TdomXPathNodeSetResult(exprResult).add(contextNode);
  end else
    exprResult := TdomXPathExpr(arguments[0]).evaluate(contextNode, contextPosition, contextSize);
  try
    if not (exprResult is TdomXPathNodeSetResult) then
      raise EXPath_Invalid_Function_Call_Err.createFmt('Argument mismatch error in %s(): Expression does not evaluate to node-set.',['name']);
    with exprResult do begin
      axisType := XPATH_FORWARD_AXIS;
      if length = 0
        then result:= TdomXPathStringResult.create('')
        else result:= TdomXPathStringResult.create(TdomNode(item(0)).expandedName);
    end;
  finally
    exprResult.free;
  end;
end;

{XPath String Functions -- see XPath 1.0, sec. 4.2.}

function XPathFunctionString(const contextNode: TdomNode;
                             const contextPosition: Integer;
                             const contextSize: Integer;
                             const arguments: TList): TdomXPathCustomResult;
var
  exprResult: TdomXPathCustomResult;
begin
  if arguments.Count > 1 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['string']);
  if arguments.Count = 0 then begin
    exprResult := TdomXPathNodeSetResult.create;
    TdomXPathNodeSetResult(exprResult).add(contextNode);
  end else
    exprResult := TdomXPathExpr(arguments[0]).evaluate(contextNode, contextPosition, contextSize);
  Result := XPathStringFunc(exprResult);  // As a side-effect automatically frees exprResult.
end;

function XPathFunctionConcat(const contextNode: TdomNode;
                             const contextPosition: Integer;
                             const contextSize: Integer;
                             const arguments: TList): TdomXPathCustomResult;
var
  exprResult: TdomXPathCustomResult;
  i: integer;
  S: wideString;
begin
  if arguments.Count < 2 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['concat']);
  S := '';
  for i := 0 to pred(arguments.count) do begin
    exprResult := TdomXPathExpr(arguments[i]).evaluate(contextNode, contextPosition, contextSize);
    try
      if not (exprResult is TdomXPathStringResult) then
        raise EXPath_Invalid_Function_Call_Err.createFmt('Argument mismatch error in %s(): Expression does not evaluate to string.',['concat']);
      S := concat(S, exprResult.asWideString);
    finally
      exprResult.free;
    end;
  end;
  result := TdomXPathStringResult.create(S);
end;

function XPathFunctionStartsWith(const contextNode: TdomNode;
                                 const contextPosition: Integer;
                                 const contextSize: Integer;
                                 const arguments: TList): TdomXPathCustomResult;
var
  S1_Result, S2_Result: TdomXPathCustomResult;
begin
  if arguments.Count <> 2 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['starts-with']);
  S2_Result := nil; // Remark: Saves one try ... finally block
  S1_Result := TdomXPathExpr(arguments[0]).evaluate(contextNode, contextPosition, contextSize);
  try
    S2_Result := TdomXPathExpr(arguments[1]).evaluate(contextNode, contextPosition, contextSize);
    if not ( (S1_Result is TdomXPathStringResult) and (S2_Result is TdomXPathStringResult) ) then
      raise EXPath_Invalid_Function_Call_Err.createFmt('Argument mismatch error in %s(): Expression does not evaluate to string.',['starts-with']);
    result := TdomXPathBooleanResult.create(
                CompareWideStr(
                  copy(S1_Result.asWideString, 1, length(S2_Result.asWideString)),
                  S2_Result.asWideString
                ) = 0
              );
  finally
    S1_Result.free;
    S2_Result.free;
  end;
end;

function XPathFunctionContains(const contextNode: TdomNode;
                               const contextPosition: Integer;
                               const contextSize: Integer;
                               const arguments: TList): TdomXPathCustomResult;
var
  S1_Result, S2_Result: TdomXPathCustomResult;
begin
  if arguments.Count <> 2 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['contains']);
  S2_Result := nil; // Remark: Saves one try ... finally block
  S1_Result := TdomXPathExpr(arguments[0]).evaluate(contextNode, contextPosition, contextSize);
  try
    S2_Result := TdomXPathExpr(arguments[1]).evaluate(contextNode, contextPosition, contextSize);
    if not ( (S1_Result is TdomXPathStringResult) and (S2_Result is TdomXPathStringResult) ) then
      raise EXPath_Invalid_Function_Call_Err.createFmt('Argument mismatch error in %s(): Expression does not evaluate to string.',['contains']);
    if length(S2_Result.asWideString) = 0
      then result := TdomXPathBooleanResult.create(true)
      else result := TdomXPathBooleanResult.create(
                       Pos(S2_Result.asWideString,S1_Result.asWideString) > 0
                     );
  finally
    S1_Result.free;
    S2_Result.free;
  end;
end;

function XPathFunctionSubstringBefore(const contextNode: TdomNode;
                                      const contextPosition: Integer;
                                      const contextSize: Integer;
                                      const arguments: TList): TdomXPathCustomResult;
var
  S1_Result, S2_Result: TdomXPathCustomResult;
begin
  if arguments.Count <> 2 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['substring-before']);
  S2_Result := nil; // Remark: Saves one try ... finally block
  S1_Result := TdomXPathExpr(arguments[0]).evaluate(contextNode, contextPosition, contextSize);
  try
    S2_Result := TdomXPathExpr(arguments[1]).evaluate(contextNode, contextPosition, contextSize);
    if not ( (S1_Result is TdomXPathStringResult) and (S2_Result is TdomXPathStringResult) ) then
      raise EXPath_Invalid_Function_Call_Err.createFmt('Argument mismatch error in %s(): Expression does not evaluate to string.',['substring-before']);
    result := TdomXPathStringResult.create(
                Copy(S1_Result.asWideString, 1,
                       Pred( Pos(S2_Result.asWideString, S1_Result.asWideString ) ) )
              );
  finally
    S1_Result.free;
    S2_Result.free;
  end;
end;

function XPathFunctionSubstringAfter(const contextNode: TdomNode;
                                     const contextPosition: Integer;
                                     const contextSize: Integer;
                                     const arguments: TList): TdomXPathCustomResult;
var
  S1_Result, S2_Result: TdomXPathCustomResult;
begin
  if arguments.Count <> 2 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['substring-after']);
  S2_Result := nil; // Remark: Saves one try ... finally block
  S1_Result := TdomXPathExpr(arguments[0]).evaluate(contextNode, contextPosition, contextSize);
  try
    S2_Result := TdomXPathExpr(arguments[1]).evaluate(contextNode, contextPosition, contextSize);
    if not ( (S1_Result is TdomXPathStringResult) and (S2_Result is TdomXPathStringResult) ) then
      raise EXPath_Invalid_Function_Call_Err.createFmt('Argument mismatch error in %s(): Expression does not evaluate to string.',['substring-after']);
    if length(S2_Result.asWideString) = 0
      then result := TdomXPathStringResult.create(S1_Result.asWideString)
      else result := TdomXPathStringResult.create(
                       Copy(S1_Result.asWideString,
                            Pos(S2_Result.asWideString, S1_Result.asWideString )
                              + length(S2_Result.asWideString),
                            length(S1_Result.asWideString)
                           )
                     );
  finally
    S1_Result.free;
    S2_Result.free;
  end;
end;

function XPathFunctionSubstring(const contextNode: TdomNode;
                                const contextPosition: Integer;
                                const contextSize: Integer;
                                const arguments: TList): TdomXPathCustomResult;
var
  S1_Result, N1_Result, N2_Result: TdomXPathCustomResult;
  I, L: integer;
begin
  if (arguments.Count <> 2) and (arguments.Count <> 3) then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['substring']);
  N1_Result := nil; // Remark: Saves one try ... finally block
  N2_Result := nil; // Remark: Saves one try ... finally block
  S1_Result := TdomXPathExpr(arguments[0]).evaluate(contextNode, contextPosition, contextSize);
  try
    N1_Result := TdomXPathExpr(arguments[1]).evaluate(contextNode, contextPosition, contextSize);
    if not (S1_Result is TdomXPathStringResult) then
      raise EXPath_Invalid_Function_Call_Err.createFmt('Argument mismatch error in %s(): Expression does not evaluate to string.',['substring']);
    if not (N1_Result is TdomXPathNumberResult) then
      raise EXPath_Invalid_Function_Call_Err.createFmt('Argument mismatch error in %s(): Expression does not evaluate to number.',['substring']);
    if arguments.Count = 3 then begin
      N2_Result := TdomXPathExpr(arguments[2]).evaluate(contextNode, contextPosition, contextSize);
      if not (N2_Result is TdomXPathNumberResult) then
        raise EXPath_Invalid_Function_Call_Err.createFmt('Argument mismatch error in %s(): Expression does not evaluate to number.',['substring']);

      if IsNaN(N1_Result.asNumber) or
         IsInfinite(N1_Result.asNumber) or
         IsNaN(N2_Result.asNumber)
      then begin
        result := TdomXPathStringResult.create('');
      end else if IsInfinite(N2_Result.asNumber) then begin
        if Sign(N2_Result.asNumber) = 1
          then result := TdomXPathStringResult.create(Copy( S1_Result.asWideString,
                                                            Trunc(XPathRound(N1_Result.asNumber)),
                                                            length(S1_Result.asWideString) ))
          else result := TdomXPathStringResult.create('');
      end else begin
        I := Max(Trunc(XPathRound((N1_Result.asNumber))), 1);
        L := Trunc(XPathRound((N1_Result.asNumber)) + XPathRound((N2_Result.asNumber))) - I;
        result := TdomXPathStringResult.create(Copy(S1_Result.asWideString, I, L) );
      end;

    end else begin

      if IsNaN(N1_Result.asNumber) then begin
        result := TdomXPathStringResult.create('');
      end else if IsInfinite(N1_Result.asNumber) then begin
        if Sign(N1_Result.asNumber) = 1
          then result := TdomXPathStringResult.create('')
          else result := TdomXPathStringResult.create(S1_Result.asWideString);
      end else
        result := TdomXPathStringResult.create(Copy( S1_Result.asWideString,
                                                     Trunc(XPathRound(N1_Result.asNumber)),
                                                     length(S1_Result.asWideString) ));

    end;
  finally
    S1_Result.free;
    N1_Result.free;
    N2_Result.free;
  end;
end;

function XPathFunctionStringLength(const contextNode: TdomNode;
                                   const contextPosition: Integer;
                                   const contextSize: Integer;
                                   const arguments: TList): TdomXPathCustomResult;
var
  exprResult: TdomXPathCustomResult;
begin
  if arguments.Count > 1 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['string-length']);
  if arguments.Count = 0 then begin
    if not assigned(contextNode) then
      raise EXPath_Invalid_Function_Call_Err.createFmt('Context node not specified for %s().',['string-length']);
    Result := TdomXPathNumberResult.create(length(contextNode.XPathStringValue));
  end else begin
    exprResult := TdomXPathExpr(arguments[0]).evaluate(contextNode, contextPosition, contextSize);
    try
      if not (exprResult is TdomXPathStringResult) then
        raise EXPath_Invalid_Function_Call_Err.createFmt('Argument mismatch error in %s(): Expression does not evaluate to string.',['string-length']);
      Result := TdomXPathNumberResult.create(length(exprResult.asWideString));
    finally
      exprResult.free;
    end;
  end;
end;

function XPathFunctionNormalizeSpace(const contextNode: TdomNode;
                                     const contextPosition: Integer;
                                     const contextSize: Integer;
                                     const arguments: TList): TdomXPathCustomResult;
var
  exprResult: TdomXPathCustomResult;
begin
  if arguments.Count > 1 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['normalize-space']);
  if arguments.Count = 0 then begin
    if not assigned(contextNode) then
      raise EXPath_Invalid_Function_Call_Err.createFmt('Context node not specified for %s().',['normalize-space']);
    Result := TdomXPathStringResult.create(normalizeWhiteSpace(contextNode.XPathStringValue));
  end else begin
    exprResult := TdomXPathExpr(arguments[0]).evaluate(contextNode, contextPosition, contextSize);
    try
      if not (exprResult is TdomXPathStringResult) then
        raise EXPath_Invalid_Function_Call_Err.createFmt('Argument mismatch error in %s(): Expression does not evaluate to string.',['normalize-space']);
      Result := TdomXPathStringResult.create(normalizeWhiteSpace(exprResult.asWideString));
    finally
      exprResult.free;
    end;
  end;
end;

function XPathFunctionTranslate(const contextNode: TdomNode;
                                const contextPosition: Integer;
                                const contextSize: Integer;
                                const arguments: TList): TdomXPathCustomResult;
var
  S1, S2, S3: TdomXPathCustomResult;
begin
  if arguments.Count <> 3 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['translate']);
  S2 := nil; // Remark: Saves one try ... finally block
  S3 := nil; // Remark: Saves one try ... finally block
  S1 := TdomXPathExpr(arguments[0]).evaluate(contextNode, contextPosition, contextSize);
  try
    S2 := TdomXPathExpr(arguments[1]).evaluate(contextNode, contextPosition, contextSize);
    S3 := TdomXPathExpr(arguments[2]).evaluate(contextNode, contextPosition, contextSize);
    if not ( (S1 is TdomXPathStringResult) and (S2 is TdomXPathStringResult) and (S3 is TdomXPathStringResult) ) then
      raise EXPath_Invalid_Function_Call_Err.createFmt('Argument mismatch error in %s(): Expression does not evaluate to string.',['translate']);
    Result := TdomXPathStringResult.create(
                translateWideString(S1.asWideString, S2.asWideString, S3.asWideString) );
  finally
    S1.free;
    S2.free;
    S3.free;
  end;
end;

{XPath Boolean Functions -- see XPath 1.0, sec. 4.3.}

function XPathFunctionBoolean(const contextNode: TdomNode;
                              const contextPosition: Integer;
                              const contextSize: Integer;
                              const arguments: TList): TdomXPathCustomResult;
var
  exprResult: TdomXPathCustomResult;
begin
  if arguments.Count <> 1 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['boolean']);
  exprResult := TdomXPathExpr(arguments[0]).evaluate(contextNode, contextPosition, contextSize);
  Result := XPathBooleanFunc(exprResult);  // As a side-effect automatically frees exprResult.
end;

function XPathFunctionNot(const contextNode: TdomNode;
                          const contextPosition: Integer;
                          const contextSize: Integer;
                          const arguments: TList): TdomXPathCustomResult;
var
  exprResult: TdomXPathCustomResult;
begin
  if arguments.Count <> 1 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['not']);
  exprResult := TdomXPathExpr(arguments[0]).evaluate(contextNode, contextPosition, contextSize);
  try
    if not (exprResult is TdomXPathBooleanResult) then
      raise EXPath_Invalid_Function_Call_Err.createFmt('Argument mismatch error in %s(): Expression does not evaluate to boolean.',['not']);
    Result := TdomXPathBooleanResult.create(not(exprResult.asBoolean));
  finally
    exprResult.free;
  end;
end;

function XPathFunctionTrue(const contextNode: TdomNode;
                           const contextPosition: Integer;
                           const contextSize: Integer;
                           const arguments: TList): TdomXPathCustomResult;
begin
  if arguments.Count > 0 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['true']);
  Result := TdomXPathBooleanResult.create(true);
end;

function XPathFunctionFalse(const contextNode: TdomNode;
                            const contextPosition: Integer;
                            const contextSize: Integer;
                            const arguments: TList): TdomXPathCustomResult;
begin
  if arguments.Count > 0 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['false']);
  Result := TdomXPathBooleanResult.create(false);
end;

function XPathFunctionLang(const contextNode: TdomNode;
                           const contextPosition: Integer;
                           const contextSize: Integer;
                           const arguments: TList): TdomXPathCustomResult;
var
  exprResult: TdomXPathCustomResult;
begin
  if arguments.Count <> 1 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['lang']);
  if not assigned(contextNode) then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Context node not specified for %s().',['lang']);
  exprResult := TdomXPathExpr(arguments[0]).evaluate(contextNode, contextPosition, contextSize);
  try
    if not (exprResult is TdomXPathStringResult) then
      raise EXPath_Invalid_Function_Call_Err.createFmt('Argument mismatch error in %s(): Expression does not evaluate to string.',['lang']);
    Result := TdomXPathBooleanResult.create(
                isSubLanguage(exprResult.asWideString, contextNode.language)
              );
  finally
    exprResult.free;
  end;
end;

{ XPath Number Functions -- see XPath 1.0, sec. 4.4. }

function XPathFunctionNumber(const contextNode: TdomNode;
                             const contextPosition: Integer;
                             const contextSize: Integer;
                             const arguments: TList): TdomXPathCustomResult;
var
  exprResult: TdomXPathCustomResult;
begin
  if arguments.Count > 1 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['number']);
  if arguments.Count = 0 then begin
    exprResult := TdomXPathNodeSetResult.create;
    TdomXPathNodeSetResult(exprResult).add(contextNode);
  end else
    exprResult := TdomXPathExpr(arguments[0]).evaluate(contextNode, contextPosition, contextSize);
  Result := XPathNumberFunc(exprResult);  // As a side-effect automatically frees exprResult.
end;

function XPathFunctionSum(const contextNode: TdomNode;
                          const contextPosition: Integer;
                          const contextSize: Integer;
                          const arguments: TList): TdomXPathCustomResult;
var
{$IFDEF VER140+}
  ExceptionMaskBackup: TFPUExceptionMask;
{$ENDIF}
  exprResult: TdomXPathCustomResult;
  I: integer;
  M, N: double;
begin
  if arguments.Count <> 1 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['sum']);
  exprResult := TdomXPathExpr(arguments[0]).evaluate(contextNode, contextPosition, contextSize);
  try
    if not (exprResult is TdomXPathNodeSetResult) then
      raise EXPath_Invalid_Function_Call_Err.createFmt('Argument mismatch error in %s(): Expression does not evaluate to node-set.',['sum']);
    N := 0;
{$IFDEF VER140+}
    ExceptionMaskBackup:= SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$ENDIF}
    try
      with TdomXPathNodeSetResult(exprResult) do
        for I := 0 to pred(length) do begin
          try
            M := XPathWideStringToNumber(item(I).XPathStringValue);
          except
            M := NaN;
          end;
          N := N + M;
        end;
{$IFDEF VER140+}
    finally
      SetExceptionMask(ExceptionMaskBackup);
{$ELSE}
    except
      N := NaN;
{$ENDIF}
    end;
    Result := TdomXPathNumberResult.create(N);
  finally
    exprResult.free;
  end;
end;

function XPathFunctionFloor(const contextNode: TdomNode;
                            const contextPosition: Integer;
                            const contextSize: Integer;
                            const arguments: TList): TdomXPathCustomResult;
var
  exprResult: TdomXPathCustomResult;
begin
  if arguments.Count <> 1 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['floor']);
  exprResult := TdomXPathExpr(arguments[0]).evaluate(contextNode, contextPosition, contextSize);
  try
    if not (exprResult is TdomXPathNumberResult) then
      raise EXPath_Invalid_Function_Call_Err.createFmt('Argument mismatch error in %s(): Expression does not evaluate to a number.',['floor']);
    with exprResult do
      if IsNaN(asNumber) or IsInfinite(asNumber)
        then Result := TdomXPathNumberResult.create(asNumber)
        else Result := TdomXPathNumberResult.create(Floor(asNumber));
  finally
    exprResult.free;
  end;
end;

function XPathFunctionCeiling(const contextNode: TdomNode;
                              const contextPosition: Integer;
                              const contextSize: Integer;
                              const arguments: TList): TdomXPathCustomResult;
var
  exprResult: TdomXPathCustomResult;
begin
  if arguments.Count <> 1 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['ceiling']);
  exprResult := TdomXPathExpr(arguments[0]).evaluate(contextNode, contextPosition, contextSize);
  try
    if not (exprResult is TdomXPathNumberResult) then
      raise EXPath_Invalid_Function_Call_Err.createFmt('Argument mismatch error in %s(): Expression does not evaluate to a number.',['ceiling']);
    with exprResult do
      if IsNaN(asNumber) or IsInfinite(asNumber)
        then Result := TdomXPathNumberResult.create(asNumber)
        else Result := TdomXPathNumberResult.create(Ceil(asNumber));
  finally
    exprResult.free;
  end;
end;

function XPathFunctionRound(const contextNode: TdomNode;
                            const contextPosition: Integer;
                            const contextSize: Integer;
                            const arguments: TList): TdomXPathCustomResult;
var
  exprResult: TdomXPathCustomResult;
begin
  if arguments.Count <> 1 then
    raise EXPath_Invalid_Function_Call_Err.createFmt('Arguments mismatch error in %s().',['round']);
  exprResult := TdomXPathExpr(arguments[0]).evaluate(contextNode, contextPosition, contextSize);
  try
    if not (exprResult is TdomXPathNumberResult) then
      raise EXPath_Invalid_Function_Call_Err.createFmt('Argument mismatch error in %s(): Expression does not evaluate to a number.',['round']);
    Result := TdomXPathNumberResult.create(XPathRound(exprResult.asNumber));
  finally
    exprResult.free;
  end;
end;

{ TdomXPathTokenizer }

constructor TdomXPathTokenizer.create(const expression: wideString;
                                      const xpathVersion: wideString);
begin
  if xpathVersion <> '1.0'
    then raise ENot_Supported_Err.CreateFmt('XPath version "%S" not supproted.',[xpathVersion]);
  FExpression:= expression;
  FLastSymbol:= XPATH_INVALID_TOKEN;  // Use XPATH_INVALID_TOKEN as a dummy value
  FPosition:= 0;
  FDoubleSlashStatus:= SL_NO_DOUBLE_SLASH;
  FPositionCache:= 0;
  FSymbolCache:= XPATH_INVALID_TOKEN;
  FValueCache:= '';
  FCacheIsActive:= false;
end;

function TdomXPathTokenizer.doubleColonFollows: boolean;
var
  i: integer;
begin
  result:= false;
  for i:= FPosition+1 to pred(length(FExpression)) do begin
    if FExpression[i] = #$3a then begin
      if FExpression[i+1] = #$3a
        then result:= true;
      exit;
    end;
    if not isXmlWhiteSpace(FExpression[i]) then exit;
  end;
end;

function TdomXPathTokenizer.getNextWideChar(out s: wideChar): boolean;
begin
  if FPosition = length(FExpression) then begin
    s:= #0;
    result:= false;
  end else begin
    inc(FPosition);
    s:= FExpression[FPosition];
    result:= true;
  end;
end;

function TdomXPathTokenizer.isFollowing(const symbol: TdomXPathTokenType): boolean;
begin
  if not FCacheIsActive then begin
    read(FSymbolCache,FValueCache,FPositionCache);
    FCacheIsActive:= true;
  end;
  if FSymbolCache = symbol
    then result:= true
    else result:= false;
end;

function TdomXPathTokenizer.leftParanthesisFollows: boolean;
var
  i: integer;
begin
  result:= false;
  for i:= FPosition+1 to length(FExpression) do begin
    if FExpression[i] = #$28 then begin
      result:= true;
      exit;
    end;
    if not isXmlWhiteSpace(FExpression[i]) then exit;
  end;
end;

function TdomXPathTokenizer.lookAheadNextWideChar(out s: wideChar): boolean;
begin
  if FPosition = length(FExpression) then begin
    s:= #0;
    result:= false;
  end else begin
    s:= FExpression[FPosition+1];
    result:= true;
  end;
end;

procedure TdomXPathTokenizer.read(out symbol: TdomXPathTokenType;
                                  out value: wideString;
                                  out position: integer);
var
  S: WideChar;
  L: WideChar;
  DecimalPointFound: boolean;
begin
  if FCacheIsActive then begin
    symbol:= FSymbolCache;
    value:= FValueCache;
    position:= FPositionCache;
    FCacheIsActive:= false;
    exit;
  end;
  case FDoubleSlashStatus of
    SL_NO_DOUBLE_SLASH: begin
      repeat
        if not getNextWideChar(S) then begin
          // End of text:
          symbol:= XPATH_END_OF_TEXT_TOKEN;
          value:= '';
          position:= -1;
          exit;
        end;
      until not isXmlWhiteSpace(S);

      case ord(S) of
        $28: begin // '('
          symbol:= XPATH_LEFT_PARENTHESIS_TOKEN;
          FLastSymbol:= XPATH_LEFT_PARENTHESIS_TOKEN;
          value:= '';
          position:= FPosition;
        end;
        $29: begin // ')'
          symbol:= XPATH_RIGHT_PARENTHESIS_TOKEN;
          FLastSymbol:= XPATH_RIGHT_PARENTHESIS_TOKEN;
          value:= '';
          position:= FPosition;
        end;
        $5b: begin // '['
          symbol:= XPATH_LEFT_SQUARE_BRACKET_TOKEN;
          FLastSymbol:= XPATH_LEFT_SQUARE_BRACKET_TOKEN;
          value:= '';
          position:= FPosition;
        end;
        $5d: begin // ']'
          symbol:= XPATH_RIGHT_SQUARE_BRACKET_TOKEN;
          FLastSymbol:= XPATH_RIGHT_SQUARE_BRACKET_TOKEN;
          value:= '';
          position:= FPosition;
        end;
        $40: begin // '@'
          symbol:= XPATH_COMMERCIAL_AT_TOKEN;
          FLastSymbol:= XPATH_COMMERCIAL_AT_TOKEN;
          value:= '';
          position:= FPosition;
        end;
        $2c: begin // ','
          symbol:= XPATH_COMMA_TOKEN;
          FLastSymbol:= XPATH_COMMA_TOKEN;
          value:= '';
          position:= FPosition;
        end;
        $3a: begin // ':'
          lookAheadNextWideChar(L);
          if L = #$3a then begin // '::'
            inc(FPosition);
            symbol:= XPATH_DOUBLE_COLON_TOKEN;
            FLastSymbol:= XPATH_DOUBLE_COLON_TOKEN;
            value:= '';
            position:= FPosition;
          end else begin
            symbol:= XPATH_INVALID_TOKEN;
            FLastSymbol:= XPATH_INVALID_TOKEN;
            value:= ':';
            position:= FPosition;
          end;
        end;
        $7c: begin // '|'
          symbol:= XPATH_SHEFFER_STROKE_OPERATOR_TOKEN;
          FLastSymbol:= XPATH_SHEFFER_STROKE_OPERATOR_TOKEN;
          value:= '';
          position:= FPosition;
        end;
        $2b: begin // '+'
          symbol:= XPATH_PLUS_OPERATOR_TOKEN;
          FLastSymbol:= XPATH_PLUS_OPERATOR_TOKEN;
          value:= '';
          position:= FPosition;
        end;
        $2d: begin // '-'
          symbol:= XPATH_MINUS_OPERATOR_TOKEN;
          FLastSymbol:= XPATH_MINUS_OPERATOR_TOKEN;
          value:= '';
          position:= FPosition;
        end;
        $3d: begin // '='
          symbol:= XPATH_IS_EQUAL_OPERATOR_TOKEN;
          FLastSymbol:= XPATH_IS_EQUAL_OPERATOR_TOKEN;
          value:= '';
          position:= FPosition;
        end;
        $21: begin // '!'
          lookAheadNextWideChar(L);
          if L = #$3d then begin // '!='
            inc(FPosition);
            symbol:= XPATH_IS_NOT_EQUAL_OPERATOR_TOKEN;
            FLastSymbol:= XPATH_IS_NOT_EQUAL_OPERATOR_TOKEN;
            value:= '';
            position:= FPosition;
          end else begin
            symbol:= XPATH_INVALID_TOKEN;
            FLastSymbol:= XPATH_INVALID_TOKEN;
            value:= '!';
            position:= FPosition;
          end;
        end;
        $2f: begin // '/'
          lookAheadNextWideChar(L);
          if L = #$2f then begin // '//'
            inc(FPosition);
            FDoubleSlashStatus:= SL_XPATH_AXIS_NAME_DESCENDANT_OR_SELF_TOKEN_FOLLOWS;
          end;
          symbol:= XPATH_SLASH_OPERATOR_TOKEN;
          FLastSymbol:= XPATH_SLASH_OPERATOR_TOKEN;
          value:= '';
          position:= FPosition;
        end;
        $3c: begin // '<'
          lookAheadNextWideChar(L);
          if L = #$3d then begin // '<='
            inc(FPosition);
            symbol:= XPATH_LESS_THAN_OR_EQUAL_OPERATOR_TOKEN;
            FLastSymbol:= XPATH_LESS_THAN_OR_EQUAL_OPERATOR_TOKEN;
          end else begin
            symbol:= XPATH_LESS_THAN_OPERATOR_TOKEN;
            FLastSymbol:= XPATH_LESS_THAN_OPERATOR_TOKEN;
          end;
          value:= '';
          position:= FPosition;
        end;
        $3e: begin // '>'
          lookAheadNextWideChar(L);
          if L = #$3d then begin // '>='
            inc(FPosition);
            symbol:= XPATH_GREATER_THAN_OR_EQUAL_OPERATOR_TOKEN;
            FLastSymbol:= XPATH_GREATER_THAN_OR_EQUAL_OPERATOR_TOKEN;
          end else begin
            symbol:= XPATH_GREATER_THAN_OPERATOR_TOKEN;
            FLastSymbol:= XPATH_GREATER_THAN_OPERATOR_TOKEN;
          end;
          value:= '';
          position:= FPosition;
        end;
        $2e: begin // '.'
          lookAheadNextWideChar(L);
          case ord(L) of
            $2e: begin // '..'
              inc(FPosition);
              symbol:= XPATH_DOUBLE_DOT_TOKEN;
              FLastSymbol:= XPATH_DOUBLE_DOT_TOKEN;
              value:= '';
              position:= FPosition;
            end;
            $30..$39: begin // Digit
              value:= '.';
              repeat
                inc(FPosition);
                value:= concat(value,wideString(L));
                lookAheadNextWideChar(L);
              until not (ord(L) in [$30..$39]);
              symbol:= XPATH_NUMBER_TOKEN;
              FLastSymbol:= XPATH_NUMBER_TOKEN;
              position:= FPosition;
            end;
          else // '.'
            symbol:= XPATH_SINGLE_DOT_TOKEN;
            FLastSymbol:= XPATH_SINGLE_DOT_TOKEN;
            value:= '';
            position:= FPosition;
          end; {case ... else}
        end;
        $30..$39: begin // Digit
          value:= S;
          DecimalPointFound:= false;
          if lookAheadNextWideChar(S) then begin
            while (ord(S) in [$30..$39]) or ((S = #$2e) and not DecimalPointFound) do begin
              inc(FPosition);
              value:= concat(value,wideString(S));
              if S = #$2e then DecimalPointFound:= true;
              lookAheadNextWideChar(S);
            end;
          end;
          symbol:= XPATH_NUMBER_TOKEN;
          FLastSymbol:= XPATH_NUMBER_TOKEN;
          position:= FPosition;
        end;
        $22: begin // '"'
          value:= '';
          if not getNextWideChar(S) then begin
            symbol:= XPATH_INVALID_TOKEN;
            FLastSymbol:= XPATH_INVALID_TOKEN;
            position:= FPosition;
            exit;
          end;
          while S <> #$22 do begin
            value:= concat(value,wideString(S));
            if not getNextWideChar(S) then begin
              symbol:= XPATH_INVALID_TOKEN;
              FLastSymbol:= XPATH_INVALID_TOKEN;
              position:= FPosition;
              exit;
            end;
          end;
          symbol:= XPATH_LITERAL_TOKEN;
          FLastSymbol:= XPATH_LITERAL_TOKEN;
          position:= FPosition;
        end;
        $27: begin // '"'
          value:= '';
          if not getNextWideChar(S) then begin
            symbol:= XPATH_INVALID_TOKEN;
            FLastSymbol:= XPATH_INVALID_TOKEN;
            position:= FPosition;
            exit;
          end;
          while S <> #$27 do begin
            value:= concat(value,wideString(S));
            if not getNextWideChar(S) then begin
              symbol:= XPATH_INVALID_TOKEN;
              FLastSymbol:= XPATH_INVALID_TOKEN;
              position:= FPosition;
              exit;
            end;
          end;
          symbol:= XPATH_LITERAL_TOKEN;
          FLastSymbol:= XPATH_LITERAL_TOKEN;
          position:= FPosition;
        end;
        $24: begin // '$'
          if not lookAheadNextWideChar(S) then begin
            symbol:= XPATH_INVALID_TOKEN;
            FLastSymbol:= XPATH_INVALID_TOKEN;
            position:= FPosition;
            exit;
          end;
          if not ( IsXmlLetter(S) or ( S = #$5f ) ) then begin  // Letter or '_'?
            symbol:= XPATH_INVALID_TOKEN;
            FLastSymbol:= XPATH_INVALID_TOKEN;
            position:= FPosition;
            value:= wideString(S);
            exit;
          end;
          value:= '';
          while IsXmlNCNameChar(S) do begin
            inc(FPosition);
            value:= concat(value,wideString(S));
            if not lookAheadNextWideChar(S)
              then break;
          end;
          if S = #$3a then begin // ':' ?
            inc(FPosition);
            if not lookAheadNextWideChar(S) then begin
              symbol:= XPATH_INVALID_TOKEN;
              FLastSymbol:= XPATH_INVALID_TOKEN;
              position:= FPosition;
              value:= concat(value,':');
              exit;
            end;
            if S = #$3a then begin // '::' ?
              dec(FPosition);
            end else begin
              value:= concat(value,':');
              if not ( IsXmlLetter(S) or ( S = #$5f ) ) then begin  // Letter or '_'?
                symbol:= XPATH_INVALID_TOKEN;
                FLastSymbol:= XPATH_INVALID_TOKEN;
                position:= FPosition;
                value:= concat(value,wideString(S));
                exit;
              end;
              while IsXmlNCNameChar(S) do begin
                inc(FPosition);
                value:= concat(value,wideString(S));
                if not self.lookAheadNextWideChar(S)
                  then break;
              end;
            end;
          end;
          symbol:= XPATH_VARIABLE_REFERENCE_TOKEN;
          FLastSymbol:= XPATH_VARIABLE_REFERENCE_TOKEN;
          position:= FPosition;
        end;
        $2a: begin // '*'
          if FLastSymbol in [ XPATH_LEFT_PARENTHESIS_TOKEN,
                              XPATH_LEFT_SQUARE_BRACKET_TOKEN,
                              XPATH_COMMERCIAL_AT_TOKEN,
                              XPATH_COMMA_TOKEN,
                              XPATH_DOUBLE_COLON_TOKEN,
                              XPATH_AND_OPERATOR_TOKEN,
                              XPATH_OR_OPERATOR_TOKEN,
                              XPATH_MOD_OPERATOR_TOKEN,
                              XPATH_DIV_OPERATOR_TOKEN,
                              XPATH_MULTIPLY_OPERATOR_TOKEN,
                              XPATH_SLASH_OPERATOR_TOKEN,
                              XPATH_SHEFFER_STROKE_OPERATOR_TOKEN,
                              XPATH_PLUS_OPERATOR_TOKEN,
                              XPATH_MINUS_OPERATOR_TOKEN,
                              XPATH_IS_EQUAL_OPERATOR_TOKEN,
                              XPATH_IS_NOT_EQUAL_OPERATOR_TOKEN,
                              XPATH_LESS_THAN_OPERATOR_TOKEN,
                              XPATH_LESS_THAN_OR_EQUAL_OPERATOR_TOKEN,
                              XPATH_GREATER_THAN_OPERATOR_TOKEN,
                              XPATH_GREATER_THAN_OR_EQUAL_OPERATOR_TOKEN,
                              XPATH_INVALID_TOKEN  // = no preceding token
                            ]
          then begin
            symbol:= XPATH_NAME_TEST_TOKEN;
            FLastSymbol:= XPATH_NAME_TEST_TOKEN;
            value:= '*';
          end else begin
            symbol:= XPATH_MULTIPLY_OPERATOR_TOKEN;
            FLastSymbol:= XPATH_MULTIPLY_OPERATOR_TOKEN;
            value:= '';
          end;
          position:= FPosition;
        end;
      else  {case ...}

        // Parse NCName:
        if not ( IsXmlLetter(S) or ( S = #$5f ) ) then begin  // Letter or '_'?
          symbol:= XPATH_INVALID_TOKEN;
          FLastSymbol:= XPATH_INVALID_TOKEN;
          position:= FPosition;
          value:= wideString(S);
          exit;
        end;
        value:= '';
        dec(FPosition);
        while IsXmlNCNameChar(S) do begin
          inc(FPosition);
          value:= concat(value,wideString(S));
          if not lookAheadNextWideChar(S)
            then break;
        end;

        if not ( FLastSymbol in [ XPATH_LEFT_PARENTHESIS_TOKEN,
                                  XPATH_LEFT_SQUARE_BRACKET_TOKEN,
                                  XPATH_COMMERCIAL_AT_TOKEN,
                                  XPATH_COMMA_TOKEN,
                                  XPATH_DOUBLE_COLON_TOKEN,
                                  XPATH_AND_OPERATOR_TOKEN,
                                  XPATH_OR_OPERATOR_TOKEN,
                                  XPATH_MOD_OPERATOR_TOKEN,
                                  XPATH_DIV_OPERATOR_TOKEN,
                                  XPATH_MULTIPLY_OPERATOR_TOKEN,
                                  XPATH_SLASH_OPERATOR_TOKEN,
                                  XPATH_SHEFFER_STROKE_OPERATOR_TOKEN,
                                  XPATH_PLUS_OPERATOR_TOKEN,
                                  XPATH_MINUS_OPERATOR_TOKEN,
                                  XPATH_IS_EQUAL_OPERATOR_TOKEN,
                                  XPATH_IS_NOT_EQUAL_OPERATOR_TOKEN,
                                  XPATH_LESS_THAN_OPERATOR_TOKEN,
                                  XPATH_LESS_THAN_OR_EQUAL_OPERATOR_TOKEN,
                                  XPATH_GREATER_THAN_OPERATOR_TOKEN,
                                  XPATH_GREATER_THAN_OR_EQUAL_OPERATOR_TOKEN,
                                  XPATH_INVALID_TOKEN  // = no preceding token
                                ] )
        then begin
          if value = 'and' then begin
            symbol:= XPATH_AND_OPERATOR_TOKEN;
            FLastSymbol:= XPATH_AND_OPERATOR_TOKEN;
            value:= '';
          end else if value = 'or' then begin
            symbol:= XPATH_OR_OPERATOR_TOKEN;
            FLastSymbol:= XPATH_OR_OPERATOR_TOKEN;
            value:= '';
          end else if value = 'mod' then begin
            symbol:= XPATH_MOD_OPERATOR_TOKEN;
            FLastSymbol:= XPATH_MOD_OPERATOR_TOKEN;
            value:= '';
          end else if value = 'div' then begin
            symbol:= XPATH_DIV_OPERATOR_TOKEN;
            FLastSymbol:= XPATH_DIV_OPERATOR_TOKEN;
            value:= '';
          end else begin
            symbol:= XPATH_INVALID_TOKEN;
            FLastSymbol:= XPATH_INVALID_TOKEN;
          end;
          position:= FPosition;
          exit;
        end;

        if doubleColonFollows then begin
          if value = 'ancestor' then begin
            symbol:= XPATH_AXIS_NAME_ANCESTOR_TOKEN;
            FLastSymbol:= XPATH_AXIS_NAME_ANCESTOR_TOKEN;
            value:= '';
          end else if value = 'ancestor-or-self' then begin
            symbol:= XPATH_AXIS_NAME_ANCESTOR_OR_SELF_TOKEN;
            FLastSymbol:= XPATH_AXIS_NAME_ANCESTOR_OR_SELF_TOKEN;
            value:= '';
          end else if value = 'attribute' then begin
            symbol:= XPATH_AXIS_NAME_ATTRIBUTE_TOKEN;
            FLastSymbol:= XPATH_AXIS_NAME_ATTRIBUTE_TOKEN;
            value:= '';
          end else if value = 'child' then begin
            symbol:= XPATH_AXIS_NAME_CHILD_TOKEN;
            FLastSymbol:= XPATH_AXIS_NAME_CHILD_TOKEN;
            value:= '';
          end else if value = 'descendant' then begin
            symbol:= XPATH_AXIS_NAME_DESCENDANT_TOKEN;
            FLastSymbol:= XPATH_AXIS_NAME_DESCENDANT_TOKEN;
            value:= '';
          end else if value = 'descendant-or-self' then begin
            symbol:= XPATH_AXIS_NAME_DESCENDANT_OR_SELF_TOKEN;
            FLastSymbol:= XPATH_AXIS_NAME_DESCENDANT_OR_SELF_TOKEN;
            value:= '';
          end else if value = 'following' then begin
            symbol:= XPATH_AXIS_NAME_FOLLOWING_TOKEN;
            FLastSymbol:= XPATH_AXIS_NAME_FOLLOWING_TOKEN;
            value:= '';
          end else if value = 'following-sibling' then begin
            symbol:= XPATH_AXIS_NAME_FOLLOWING_SIBLING_TOKEN;
            FLastSymbol:= XPATH_AXIS_NAME_FOLLOWING_SIBLING_TOKEN;
            value:= '';
          end else if value = 'namespace' then begin
            symbol:= XPATH_AXIS_NAME_NAMESPACE_TOKEN;
            FLastSymbol:= XPATH_AXIS_NAME_NAMESPACE_TOKEN;
            value:= '';
          end else if value = 'parent' then begin
            symbol:= XPATH_AXIS_NAME_PARENT_TOKEN;
            FLastSymbol:= XPATH_AXIS_NAME_PARENT_TOKEN;
            value:= '';
          end else if value = 'preceding' then begin
            symbol:= XPATH_AXIS_NAME_PRECEDING_TOKEN;
            FLastSymbol:= XPATH_AXIS_NAME_PRECEDING_TOKEN;
          end else if value = 'preceding-sibling' then begin
            symbol:= XPATH_AXIS_NAME_PRECEDING_SIBLING_TOKEN;
            FLastSymbol:= XPATH_AXIS_NAME_PRECEDING_SIBLING_TOKEN;
            value:= '';
          end else if value = 'self' then begin
            symbol:= XPATH_AXIS_NAME_SELF_TOKEN;
            FLastSymbol:= XPATH_AXIS_NAME_SELF_TOKEN;
            value:= '';
          end else begin
            symbol:= XPATH_INVALID_TOKEN;
            FLastSymbol:= XPATH_INVALID_TOKEN;
            value:= '';
          end;
          position:= FPosition;
          exit;
        end;

        if S = #$3a then begin // ':' ?
          inc(FPosition);
          if not lookAheadNextWideChar(S) then begin
            symbol:= XPATH_INVALID_TOKEN;
            FLastSymbol:= XPATH_INVALID_TOKEN;
            position:= FPosition;
            value:= concat(value,':');
            exit;
          end;
          if S = #$3a then begin // '::' ?
            dec(FPosition);
          end else begin
            value:= concat(value,':');
            if not ( IsXmlLetter(S) or ( S = #$5f ) ) then begin  // Letter or '_'?
              if S = #$2a then begin // '*
                symbol:= XPATH_NAME_TEST_TOKEN;
                FLastSymbol:= XPATH_NAME_TEST_TOKEN;
              end else begin
                symbol:= XPATH_INVALID_TOKEN;
                FLastSymbol:= XPATH_INVALID_TOKEN;
              end;
              inc(FPosition);
              position:= FPosition;
              value:= concat(value,wideString(S));
              exit;
            end;
            while IsXmlNCNameChar(S) do begin
              inc(FPosition);
              value:= concat(value,wideString(S));
              if not self.lookAheadNextWideChar(S)
                then break;
            end;
          end;
        end;

        if leftParanthesisFollows then begin
          if value = 'comment' then begin
            symbol:= XPATH_NODE_TYPE_COMMENT_TOKEN;
            FLastSymbol:= XPATH_NODE_TYPE_COMMENT_TOKEN;
            value:= '';
          end else if value = 'text' then begin
            symbol:= XPATH_NODE_TYPE_TEXT_TOKEN;
            FLastSymbol:= XPATH_NODE_TYPE_TEXT_TOKEN;
            value:= '';
          end else if value = 'processing-instruction' then begin
            symbol:= XPATH_NODE_TYPE_PI_TOKEN;
            FLastSymbol:= XPATH_NODE_TYPE_PI_TOKEN;
            value:= '';
          end else if value = 'node' then begin
            symbol:= XPATH_NODE_TYPE_NODE_TOKEN;
            FLastSymbol:= XPATH_NODE_TYPE_NODE_TOKEN;
            value:= '';
          end else begin
            symbol:= XPATH_FUNCTION_NAME_TOKEN;
            FLastSymbol:= XPATH_FUNCTION_NAME_TOKEN;
          end;
        end else begin
          symbol:= XPATH_NAME_TEST_TOKEN;
          FLastSymbol:= XPATH_NAME_TEST_TOKEN;
        end;
        position:= FPosition;

      end; {case ... else ...}

    end;
    SL_XPATH_AXIS_NAME_DESCENDANT_OR_SELF_TOKEN_FOLLOWS: begin
      symbol:= XPATH_AXIS_NAME_DESCENDANT_OR_SELF_TOKEN;
      // FLastSymbol:= XPATH_AXIS_NAME_DESCENDANT_OR_SELF_TOKEN;
      // FLastSymbol will never be evaluated, so we do not need to set it.
      position:= FPosition;
      value:= '';
      FDoubleSlashStatus:= SL_XPATH_DOUBLE_COLON_TOKEN_FOLLOWS;
    end;
    SL_XPATH_DOUBLE_COLON_TOKEN_FOLLOWS: begin
      symbol:= XPATH_DOUBLE_COLON_TOKEN;
      // FLastSymbol:= XPATH_DOUBLE_COLON_TOKEN;
      // FLastSymbol will never be evaluated, so we do not need to set it.
      position:= FPosition;
      value:= '';
      FDoubleSlashStatus:= SL_XPATH_NODE_TYPE_NODE_TOKEN_FOLLOWS;
    end;
    SL_XPATH_NODE_TYPE_NODE_TOKEN_FOLLOWS: begin
      symbol:= XPATH_NODE_TYPE_NODE_TOKEN;
      // FLastSymbol:= XPATH_NODE_TYPE_NODE_TOKEN;
      // FLastSymbol will never be evaluated, so we do not need to set it.
      position:= FPosition;
      value:= '';
      FDoubleSlashStatus:= SL_XPATH_LEFT_PARENTHESIS_FOLLOWS;
    end;
    SL_XPATH_LEFT_PARENTHESIS_FOLLOWS: begin
      symbol:= XPATH_LEFT_PARENTHESIS_TOKEN;
      // FLastSymbol:= XPATH_LEFT_PARENTHESIS_TOKEN;
      // FLastSymbol will never be evaluated, so we do not need to set it.
      position:= FPosition;
      value:= '';
      FDoubleSlashStatus:= SL_XPATH_RIGHT_PARENTHESIS_FOLLOWS;
    end;
    SL_XPATH_RIGHT_PARENTHESIS_FOLLOWS: begin
      symbol:= XPATH_RIGHT_PARENTHESIS_TOKEN;
      // FLastSymbol:= XPATH_RIGHT_PARENTHESIS_TOKEN;
      // FLastSymbol will never be evaluated, so we do not need to set it.
      position:= FPosition;
      value:= '';
      FDoubleSlashStatus:= SL_XPATH_SLASH_OPERATOR_TOKEN_FOLLLOWS;
    end;
    SL_XPATH_SLASH_OPERATOR_TOKEN_FOLLLOWS: begin
      symbol:= XPATH_SLASH_OPERATOR_TOKEN;
      FLastSymbol:= XPATH_SLASH_OPERATOR_TOKEN;
      position:= FPosition;
      value:= '';
      FDoubleSlashStatus:= SL_NO_DOUBLE_SLASH;
    end;
  end; {case FDoubleSlashStatus ...}
end;

procedure TdomXPathTokenizer.reset;
begin
  FCacheIsActive:= false;
  FLastSymbol:= XPATH_INVALID_TOKEN;  // Use XPATH_INVALID_TOKEN as a dummy value
  FPosition:= 0;
  FDoubleSlashStatus:= SL_NO_DOUBLE_SLASH;
end;

{ TdomXPathCustomResult }

constructor TdomXPathCustomResult.create;
begin
  inherited Create(nil);
end;

function TdomXPathCustomResult.getAxisType: TdomXPathAxisType;
begin
  result := XPATH_FORWARD_AXIS;
end;

function TdomXPathCustomResult.item(const index: integer): TdomNode;
begin
  result := nil;
end;

function TdomXPathCustomResult.length: integer;
begin
  result := 0;
end;

procedure TdomXPathCustomResult.setAxisType(const Value: TdomXPathAxisType);
begin
  // By default do nothing.
end;

{ TdomXPathNodeSetResult }

constructor TdomXPathNodeSetResult.create;
begin
  inherited Create;
  FAxisType:= XPATH_FORWARD_AXIS;
  FList:= TList.create;
end;

destructor TdomXPathNodeSetResult.destroy;
begin
  FList.free;
  inherited;
end;

procedure TdomXPathNodeSetResult.add(const node: TdomNode);
begin
  if node.nodeType = ntXPath_Namespace_Node
    then with node as TdomXPathNamespace do
      addXPathNamespace(OwnerElement, NamespaceUri, Prefix)
    else FList.Add(node);
end;

procedure TdomXPathNodeSetResult.addSubtree(const node: TdomNode);
// Adds 'node' and its subtree, excluding attributes.
var
  n: TdomNode;
  bufferList: TList;
  i: integer;
begin
  if axisType = XPATH_FORWARD_AXIS then begin
    if assigned(node) then begin
      with node.referenceDocument.createNodeIterator( node,
                                                  [ ntElement_Node,
                                                    ntText_Node,
                                                    ntCDATA_Section_Node,
                                                    ntEntity_Reference_Node,
                                                    ntProcessing_Instruction_Node,
                                                    ntComment_Node,
                                                    ntDocument_Node ],
                                                  nil,
                                                  false ) do begin
        n:= NextNode;
        while assigned(n) do begin
          FList.add(n);
          n:= NextNode;
        end;
        detach;
      end;
      node.referenceDocument.clearInvalidNodeIterators;
    end;
  end else begin
    if assigned(node) then begin
      bufferList:= TList.create;
      try
        with node.referenceDocument.createNodeIterator( node,
                                                    [ ntElement_Node,
                                                      ntText_Node,
                                                      ntCDATA_Section_Node,
                                                      ntEntity_Reference_Node,
                                                      ntProcessing_Instruction_Node,
                                                      ntComment_Node,
                                                      ntDocument_Node ],
                                                    nil,
                                                    false ) do begin
          n:= NextNode;
          while assigned(n) do begin
            bufferList.add(n);
            n:= NextNode;
          end;
          detach;
        end;
        node.referenceDocument.clearInvalidNodeIterators;

        for i:= pred(bufferList.count) downto 0 do
          FList.add(bufferList[i]);

      finally
        bufferList.free;
      end;
    end;
  end;
end;

procedure TdomXPathNodeSetResult.addXPathNamespace(const aOwnerElement: TdomElement;
                                                    const aNamespaceUri,
                                                          aPrefix: wideString);
begin
  FList.Add(createXPathNamespace(AOwnerElement, ANamespaceUri, APrefix));
end;

function TdomXPathNodeSetResult.asBoolean: boolean;
begin
  result:= length > 0;
end;

function TdomXPathNodeSetResult.asNumber: double;
begin
  result:= XPathWideStringToNumber(asWideString);
end;

function TdomXPathNodeSetResult.asWideString: wideString;
begin
  if length = 0 then begin
    result:= ''
  end else if axisType = XPATH_FORWARD_AXIS then begin
    result:= item(0).XPathStringValue;
  end else result:= item(length).XPathStringValue;
end;

function TdomXPathNodeSetResult.createXPathNamespace(const aOwnerElement: TdomElement;
                                                     const aNamespaceUri,
                                                           aPrefix: wideString): TdomXPathNamespace;
begin
  Result:= TdomXPathNamespace.create(self, aOwnerElement, aNamespaceUri, aPrefix);
end;

procedure TdomXPathNodeSetResult.clear;
var
  i: integer;
begin
  // Free all XPath Namespace nodes:
  for i:= 0 to pred(FList.count) do
    if TdomNode(FList[i]).nodeType = ntXPath_Namespace_Node then
      TdomNode(FList[i]).free;
  FList.clear;
end;

procedure TdomXPathNodeSetResult.Delete(const index: integer);
begin
  // If the node is an XPath Namespace node then free it:
  if TdomNode(FList[index]).nodeType = ntXPath_Namespace_Node then
    TdomNode(FList[index]).free;
  FList.Delete(index);
end;

function TdomXPathNodeSetResult.getAxisType: TdomXPathAxisType;
begin
  result:= FAxisType;
end;

procedure TdomXPathNodeSetResult.insert(const index: integer;
                                        const node: TdomNode);
begin
  if node.nodeType = ntXPath_Namespace_Node
    then with node as TdomXPathNamespace do
      FList.Insert(index, createXPathNamespace(OwnerElement, NamespaceUri, Prefix))
    else FList.Insert(index, node);
end;

function TdomXPathNodeSetResult.item(const index: integer): TdomNode;
begin
  if (index < 0) or (index >= FList.Count)
    then result:= nil
    else result:= TdomNode(FList.List^[index]);
end;

function TdomXPathNodeSetResult.length: integer;
begin
  result:= FList.count;
end;

procedure TdomXPathNodeSetResult.merge(const nodeSet: TdomXPathNodeSetResult);
// Merges two sorted TdomXPathNodeSetResult objects.
var
  i, x, y: integer;
  treePosition: TdomTreePosition;
  equivalentItems: TList;
begin
  if nodeSet = self then exit;
  nodeSet.axisType:= axisType;
  x:= 0;
  y:= 0;
  equivalentItems:= TList.create;
  try

    if axisType = XPATH_FORWARD_AXIS then begin
      while (x < length) and (y < nodeSet.length) do begin
        treePosition:= item(x).compareTreePosition(nodeSet.item(y));
        if (Tree_Position_Same_Node in treePosition) then begin
          inc(y);
        end else if (Tree_Position_Equivalent in treePosition) then begin
          equivalentItems.Add(nodeSet.item(y));
          inc(y);
        end else if (Tree_Position_Following in treePosition) then begin
          inc(x);
          for i:= pred(equivalentItems.Count) downto 0 do
            if (Tree_Position_Same_Node in item(x).compareTreePosition(equivalentItems[i]))
              then equivalentItems.Delete(i);
        end else if (Tree_Position_Disconnected in treePosition) then begin
          for i:= 0 to pred(equivalentItems.Count) do begin
            insert(x,equivalentItems[i]);
            equivalentItems.Delete(i);
            inc(x);
          end;
          inc(x);
        end else begin
          for i:= 0 to pred(equivalentItems.Count) do begin
            insert(x,equivalentItems[i]);
            equivalentItems.Delete(i);
            inc(x);
          end;
          insert(x,nodeSet.item(y));
          inc(x);
          inc(y);
        end;
      end;
    end else begin
      while (x < length) and (y < nodeSet.length) do begin
        treePosition:= item(x).compareTreePosition(nodeSet.item(y));
        if (Tree_Position_Same_Node in treePosition) then begin
          inc(y);
        end else if (Tree_Position_Equivalent in treePosition) then begin
          equivalentItems.Add(nodeSet.item(y));
          inc(y);
        end else if (Tree_Position_Preceding in treePosition) then begin
          inc(x);
          for i:= pred(equivalentItems.Count) downto 0 do
            if (Tree_Position_Same_Node in item(x).compareTreePosition(equivalentItems[i]))
              then equivalentItems.Delete(i);
        end else if (Tree_Position_Disconnected in treePosition) then begin
          for i:= 0 to pred(equivalentItems.Count) do begin
            insert(x,equivalentItems[i]);
            equivalentItems.Delete(i);
            inc(x);
          end;
          inc(x);
        end else begin
          for i:= 0 to pred(equivalentItems.Count) do begin
            insert(x,equivalentItems[i]);
            equivalentItems.Delete(i);
            inc(x);
          end;
          insert(x,nodeSet.item(y));
          inc(x);
          inc(y);
        end;
      end;
    end;

    inc(x);
    while (equivalentItems.Count > 0) and (x < length) do begin
      if not (Tree_Position_Equivalent in item(x).compareTreePosition(equivalentItems[0])) then begin
        for i:= 0 to pred(equivalentItems.Count) do begin
          insert(x,equivalentItems[i]);
          equivalentItems.Delete(i);
        end;
      end;
      for i:= pred(equivalentItems.Count) downto 0 do
        if (Tree_Position_Same_Node in item(x).compareTreePosition(equivalentItems[i]))
          then equivalentItems.Delete(i);
      inc(x);
    end;

    for i:= 0 to pred(equivalentItems.Count) do
      add(equivalentItems[i]);

    if y < nodeSet.length then
      for i:= y to pred(nodeSet.length) do
        add(nodeSet.item(i));

  finally
    equivalentItems.free;
  end;
end;

function TdomXPathNodeSetResult.resultType: TdomXPathResultType;
begin
  result:= XPATH_NODE_SET_TYPE;
end;

procedure TdomXPathNodeSetResult.setAxisType(const value: TdomXPathAxisType);
var
  item: Pointer;
  index1,index2,j: integer;
begin
  If FAxisType <> value then begin
    FAxisType:= value;
    j:= pred(FList.Count);
    for index1:= 0 to ( j shr 1 ) do begin
      index2:= j - index1;
      item:= FList.List^[index1];
      FList.List^[index1]:= FList.List^[index2];
      FList.List^[index2]:= item;
      // Remark: I could have used FList.exchange(index1, index2) instead,
      // but re-implementing the swaping is faster, because parameter
      // tests are avoided.
    end;
  end;
end;

procedure TdomXPathNodeSetResult.Assign(Source: TPersistent);
var
  i: integer;
begin
  if Source is TdomXPathNodeSetResult then begin
    if Source = self then exit;
    clear;
    axisType:= TdomXPathNodeSetResult(Source).axisType;
    for i:= 0 to pred(TdomXPathNodeSetResult(Source).length) do
      add(TdomXPathNodeSetResult(Source).item(i));
  end else if Source is TXPathExpression then begin
    if TXPathExpression(Source).FXPathResult = self then exit;
    clear;
    axisType:= TXPathExpression(Source).resultAxisType;
    for i:= 0 to pred(TXPathExpression(Source).resultLength) do
      add(TXPathExpression(Source).resultNode(i));
  end else inherited Assign(Source);
end;

{ TdomXPathBooleanResult }

constructor TdomXPathBooleanResult.create(const aBooleanValue: boolean);
begin
  inherited Create;
  FBooleanValue:= aBooleanValue;
end;

function TdomXPathBooleanResult.asBoolean: boolean;
begin
  result := FBooleanValue;
end;

function TdomXPathBooleanResult.asNumber: double;
begin
  if asBoolean
    then result:= 1
    else result:= 0;
end;

function TdomXPathBooleanResult.asWideString: wideString;
begin
  if asBoolean
    then result:= 'true'
    else result:= 'false';
end;

function TdomXPathBooleanResult.resultType: TdomXPathResultType;
begin
  result:= XPATH_BOOLEAN_TYPE;
end;

{ TdomXPathNumberResult }

constructor TdomXPathNumberResult.create(const aNumberValue: double);
begin
  inherited Create;
  FNumberValue:= aNumberValue;
end;

function TdomXPathNumberResult.asBoolean: boolean;
begin
  result := not( (asNumber = 0) or isNaN(asNumber) );
end;

function TdomXPathNumberResult.asNumber: double;
begin
  result:= FNumberValue;
end;

function TdomXPathNumberResult.asWideString: wideString;
begin
  if isNaN(asNumber) then begin
    result:= 'NaN';
  end else if IsInfinite(asNumber) then begin
    if sign(asNumber) = 1
      then result:= 'Infinity'
      else result:= '-Infinity';
  end else result:= FloatToStr(asNumber);
end;

function TdomXPathNumberResult.resultType: TdomXPathResultType;
begin
  result:= XPATH_NUMBER_TYPE;
end;

{ TdomXPathStringResult }

constructor TdomXPathStringResult.create(const aStringValue: wideString);
begin
  inherited Create;
  FStringValue:= aStringValue;
end;

function TdomXPathStringResult.asBoolean: boolean;
begin
  result := system.length(asWideString) > 0;
end;

function TdomXPathStringResult.asNumber: double;
begin
  result:= XPathWideStringToNumber(asWideString);
end;

function TdomXPathStringResult.asWideString: wideString;
begin
  result := FStringValue;
end;

function TdomXPathStringResult.resultType: TdomXPathResultType;
begin
  result:= XPATH_STRING_TYPE;
end;

{ TdomXPathSyntaxTree }

constructor TdomXPathSyntaxTree.create(aOwner: TXPathExpression);
begin
  inherited create(nil);
  FOwnerXPathExpression := AOwner;
end;

procedure TdomXPathSyntaxTree.clear;
begin
  inherited;
  FRootExpr := nil; // Remark: FRootExpr was freed in the inherited clear procedure.
end;

function TdomXPathSyntaxTree.createSyntaxNode(const symbol: TdomXPathTokenType;
                                              const value: wideString): TdomXPathSyntaxNode;
begin
  case symbol of
    XPATH_LEFT_PARENTHESIS_TOKEN:
      result:= TdomXPathLeftParenthesis.create(self, value);
    XPATH_RIGHT_PARENTHESIS_TOKEN:
      result:= TdomXPathRightParenthesis.create(self, value);
    XPATH_LEFT_SQUARE_BRACKET_TOKEN:
      result:= TdomXPathLeftSquareBracket.create(self, value);
    XPATH_RIGHT_SQUARE_BRACKET_TOKEN:
      result:= TdomXPathRightSquareBracket.create(self, value);
    XPATH_SINGLE_DOT_TOKEN:
      result:= TdomXPathSingleDot.create(self, value);
    XPATH_DOUBLE_DOT_TOKEN:
      result:= TdomXPathDoubleDot.create(self, value);
    XPATH_COMMERCIAL_AT_TOKEN:
      result:= TdomXPathCommercialAt.create(self, value);
    XPATH_COMMA_TOKEN:
      result:= TdomXPathComma.create(self, value);
    XPATH_DOUBLE_COLON_TOKEN:
      result:= TdomXPathDoubleColon.create(self, value);
    XPATH_NAME_TEST_TOKEN:
      result:= TdomXPathNameTest.create(self, value);
    XPATH_NODE_TYPE_COMMENT_TOKEN:
      result:= TdomXPathNodeTypeComment.create(self, value);
    XPATH_NODE_TYPE_TEXT_TOKEN:
      result:= TdomXPathNodeTypeText.create(self, value);
    XPATH_NODE_TYPE_PI_TOKEN:
      result:= TdomXPathNodeTypePI.create(self, value);
    XPATH_NODE_TYPE_NODE_TOKEN:
      result:= TdomXPathNodeTypeNode.create(self, value);
    XPATH_AND_OPERATOR_TOKEN:
      result:= TdomXPathAndOperator.create(self, value);
    XPATH_OR_OPERATOR_TOKEN:
      result:= TdomXPathOrOperator.create(self, value);
    XPATH_MOD_OPERATOR_TOKEN:
      result:= TdomXPathModOperator.create(self, value);
    XPATH_DIV_OPERATOR_TOKEN:
      result:= TdomXPathDivOperator.create(self, value);
    XPATH_MULTIPLY_OPERATOR_TOKEN:
      result:= TdomXPathMultiplyOperator.create(self, value);
    XPATH_SLASH_OPERATOR_TOKEN:
      result:= TdomXPathSlashOperator.create(self, value);
    XPATH_SHEFFER_STROKE_OPERATOR_TOKEN:
      result:= TdomXPathShefferStrokeOperator.create(self, value);
    XPATH_PLUS_OPERATOR_TOKEN:
      result:= TdomXPathPlusOperator.create(self, value);
    XPATH_MINUS_OPERATOR_TOKEN:
      result:= TdomXPathMinusOperator.create(self, value);
    XPATH_IS_EQUAL_OPERATOR_TOKEN:
      result:= TdomXPathIsEqualOperator.create(self, value);
    XPATH_IS_NOT_EQUAL_OPERATOR_TOKEN:
      result:= TdomXPathIsNotEqualOperator.create(self, value);
    XPATH_LESS_THAN_OPERATOR_TOKEN:
      result:= TdomXPathLessThanOperator.create(self, value);
    XPATH_LESS_THAN_OR_EQUAL_OPERATOR_TOKEN:
      result:= TdomXPathLessThanOrEqualOperator.create(self, value);
    XPATH_GREATER_THAN_OPERATOR_TOKEN:
      result:= TdomXPathGreaterThanOperator.create(self, value);
    XPATH_GREATER_THAN_OR_EQUAL_OPERATOR_TOKEN:
      result:= TdomXPathGreaterThanOrEqualOperator.create(self, value);
    XPATH_FUNCTION_NAME_TOKEN:
      result:= TdomXPathFunctionName.create(self, value); 
    XPATH_AXIS_NAME_ANCESTOR_TOKEN:
      result:= TdomXPathAxisNameAncestor.create(self, value);
    XPATH_AXIS_NAME_ANCESTOR_OR_SELF_TOKEN:
      result:= TdomXPathAxisNameAncestorOrSelf.create(self, value);
    XPATH_AXIS_NAME_ATTRIBUTE_TOKEN:
      result:= TdomXPathAxisNameAttribute.create(self, value);
    XPATH_AXIS_NAME_CHILD_TOKEN:
      result:= TdomXPathAxisNameChild.create(self, value);
    XPATH_AXIS_NAME_DESCENDANT_TOKEN:
      result:= TdomXPathAxisNameDescendant.create(self, value);
    XPATH_AXIS_NAME_DESCENDANT_OR_SELF_TOKEN:
      result:= TdomXPathAxisNameDescendantOrSelf.create(self, value);
    XPATH_AXIS_NAME_FOLLOWING_TOKEN:
      result:= TdomXPathAxisNameFollowing.create(self, value);
    XPATH_AXIS_NAME_FOLLOWING_SIBLING_TOKEN:
      result:= TdomXPathAxisNameFollowingSibling.create(self, value);
    XPATH_AXIS_NAME_NAMESPACE_TOKEN:
      result:= TdomXPathAxisNameNamespace.create(self, value);
    XPATH_AXIS_NAME_PARENT_TOKEN:
      result:= TdomXPathAxisNameParent.create(self, value);
    XPATH_AXIS_NAME_PRECEDING_TOKEN:
      result:= TdomXPathAxisNamePreceding.create(self, value);
    XPATH_AXIS_NAME_PRECEDING_SIBLING_TOKEN:
      result:= TdomXPathAxisNamePrecedingSibling.create(self, value);
    XPATH_AXIS_NAME_SELF_TOKEN:
      result:= TdomXPathAxisNameSelf.create(self, value);
    XPATH_LITERAL_TOKEN:
      result:= TdomXPathLiteral.create(self, value);
    XPATH_NUMBER_TOKEN:
      result:= TdomXPathNumber.create(self, value);
    XPATH_VARIABLE_REFERENCE_TOKEN:
      result:= TdomXPathVariableReference.create(self, value);
  else
    result:= nil;
  end;
end;

function TdomXPathSyntaxTree.evaluate: TdomXPathCustomResult;
begin
  if assigned(contextNode) then
    if not (contextNode.nodeType in [ ntElement_Node,
                                      ntAttribute_Node,
                                      ntText_Node,
                                      ntProcessing_Instruction_Node,
                                      ntComment_Node,
                                      ntDocument_Node,
                                      ntXPath_Namespace_Node ] ) then
        raise ENot_Supported_Err.create('Not supported error.');
  if not assigned(FRootExpr) then
    raise EXPath_Invalid_Expression_Err.create('No valid XPath expression prepared.');
  result:= FRootExpr.evaluate(contextNode, 1, 1);
end;

function TdomXPathSyntaxTree.getIsPrepared: boolean;
begin
  result:= assigned(FRootExpr);
end;

function TdomXPathSyntaxTree.prepare(const expression: wideString): boolean;
var
  position: integer;
  stack: TdomXPathSyntaxNodeStack;
  symbol: TdomXPathTokenType;
  tokenizer: TdomXPathTokenizer;
  value: wideString;
  axisNode: TdomXPathSyntaxNode;
  lastSyntaxNode: TdomXPathSyntaxNode;
  newSyntaxNode: TdomXPathSyntaxNode;
  nodeTestNode: TdomXPathSyntaxNode;
  functionCallNode: TdomXPathFunctionCall;
  nodeTypePI: TdomXPathSyntaxNode;
  PILiteral: TdomXPathSyntaxNode;
begin
  clear; // Free the root expression, if any, and all its children.
  tokenizer:= TdomXPathTokenizer.create(expression,'1.0');
  try
    stack:= TdomXPathSyntaxNodeStack.create;
    try
      repeat
        tokenizer.read(symbol, value, position);
        case symbol of
        XPATH_END_OF_TEXT_TOKEN, XPATH_INVALID_TOKEN: break;
        else
          lastSyntaxNode:= createSyntaxNode(symbol, value);
          repeat
            // -- if lastSyntaxNode is TdomXPathAbsoluteLocationPath then ... --
            // (TdomXPathAbsoluteLocationPath will not appear in this loop,
            // so we leave it out here.)
            if lastSyntaxNode is TdomXPathAndExpr then begin
              if tokenizer.isFollowing(XPATH_SHEFFER_STROKE_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_MULTIPLY_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_DIV_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_MOD_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_PLUS_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_MINUS_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_LESS_THAN_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_LESS_THAN_OR_EQUAL_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_GREATER_THAN_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_GREATER_THAN_OR_EQUAL_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_IS_EQUAL_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_IS_NOT_EQUAL_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_AND_OPERATOR_TOKEN)
              then begin
                // Operator of higher precedence is following, so we postpone building the expression.
                stack.push(lastSyntaxNode);
                break;
              end;
              if (stack.peek(0) is TdomXPathOrOperator) and
                 ( (stack.peek(1) is TdomXPathOrExpr) )
              then begin
                // XPath 1.0, prod. [21]:
                stack.pop.free;
                newSyntaxNode:= TdomXPathOrExpr.create(self, ''); // Create OrExpr.
                newSyntaxNode.left:= stack.pop;                   // Append OrExpr.
                newSyntaxNode.right:= lastSyntaxNode;             // Append AndExpr.
                lastSyntaxNode:= newSyntaxNode;
              end else begin
                // XPath 1.0, prod. [21]:
                newSyntaxNode:= TdomXPathOrExpr.create(self, ''); // Create OrExpr.
                newSyntaxNode.left:= lastSyntaxNode;              // Append AndExpr.
                lastSyntaxNode:= newSyntaxNode;
              end;
            end else if (lastSyntaxNode is TdomXPathAndOperator) or
                        (lastSyntaxNode is TdomXPathComma) or
                        (lastSyntaxNode is TdomXPathCommercialAt) or
                        (lastSyntaxNode is TdomXPathCustomAxisName)
            then begin
              stack.push(lastSyntaxNode);
              break;
            end else if (lastSyntaxNode is TdomXPathDivExpr) or
                        (lastSyntaxNode is TdomXPathModExpr) or
                        (lastSyntaxNode is TdomXPathMultiplyExpr)
            then begin
              if tokenizer.isFollowing(XPATH_SHEFFER_STROKE_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_MULTIPLY_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_DIV_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_MOD_OPERATOR_TOKEN)
              then begin
                // Operator of higher precedence is following, so we postpone building the expression.
                stack.push(lastSyntaxNode);
                break;
              end;
              if (stack.peek(0) is TdomXPathPlusOperator) and
                 ( (stack.peek(1) is TdomXPathPlusExpr) or
                   (stack.peek(1) is TdomXPathMinusExpr) )
              then begin
                // XPath 1.0, prod. [25]:
                stack.pop.free;
                newSyntaxNode:= TdomXPathPlusExpr.create(self, ''); // Create PlusExpr.
                newSyntaxNode.left:= stack.pop;                     // Append AdditiveExpr.
                newSyntaxNode.right:= lastSyntaxNode;               // Append MultiplicativeExpr.
                lastSyntaxNode:= newSyntaxNode;
              end else if (stack.peek(0) is TdomXPathMinusOperator) and
                 ( (stack.peek(1) is TdomXPathPlusExpr) or
                   (stack.peek(1) is TdomXPathMinusExpr) )
              then begin
                // XPath 1.0, prod. [25]:
                stack.pop.free;
                newSyntaxNode:= TdomXPathMinusExpr.create(self, ''); // Create MinusExpr.
                newSyntaxNode.left:= stack.pop;                      // Append AdditiveExpr.
                newSyntaxNode.right:= lastSyntaxNode;                // Append MultiplicativeExpr.
                lastSyntaxNode:= newSyntaxNode;
              end else begin
                // XPath 1.0, prod. [25]:
                newSyntaxNode:= TdomXPathPlusExpr.create(self, ''); // Create PlusExpr.
                newSyntaxNode.left:= lastSyntaxNode;                // Append MultiplicativeExpr.
                lastSyntaxNode:= newSyntaxNode;
              end;
            end else if (lastSyntaxNode is TdomXPathDivOperator) or
                        (lastSyntaxNode is TdomXPathDoubleColon)
            then begin
              stack.push(lastSyntaxNode);
              break;
            end else if lastSyntaxNode is TdomXPathDoubleDot then begin
              // XPath 1.0, prod. [12]:
              lastSyntaxNode.free;
              lastSyntaxNode:= TdomXPathStep.create(self, '');
              lastSyntaxNode.left:= TdomXPathAxisNameParent.create(self, '');
              lastSyntaxNode.left.left:= TdomXPathNodeTest.create(self, '');
              lastSyntaxNode.left.left.left:= TdomXPathNodeTypeNode.create(self, '');
            end else if lastSyntaxNode is TdomXPathExpr then begin
              stack.push(lastSyntaxNode);
              break;
            end else if lastSyntaxNode is TdomXPathFilterExpr then begin
              // XPath 1.0, prod. [19]:
              if tokenizer.isFollowing(XPATH_SLASH_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_LEFT_SQUARE_BRACKET_TOKEN)
              then begin
                // A Slash or Predicate is following, so we postpone building the PathExpr.
                stack.push(lastSyntaxNode);
                break;
              end;
              newSyntaxNode:= TdomXPathPathExpr.create(self, ''); // Create PathExpr.
              newSyntaxNode.left:= lastSyntaxNode;                // Append FilterExpr.
              lastSyntaxNode:= newSyntaxNode;
            end else if lastSyntaxNode is TdomXPathFunctionName then begin
              stack.push(lastSyntaxNode);
              break;
            end else if (lastSyntaxNode is TdomXPathGreaterThanExpr) or
                        (lastSyntaxNode is TdomXPathGreaterThanOrEqualExpr) or
                        (lastSyntaxNode is TdomXPathLessThanExpr) or
                        (lastSyntaxNode is TdomXPathLessThanOrEqualExpr)
            then begin
              if tokenizer.isFollowing(XPATH_SHEFFER_STROKE_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_MULTIPLY_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_DIV_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_MOD_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_PLUS_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_MINUS_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_LESS_THAN_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_LESS_THAN_OR_EQUAL_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_GREATER_THAN_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_GREATER_THAN_OR_EQUAL_OPERATOR_TOKEN)
              then begin
                // Operator of higher precedence is following, so we postpone building the expression.
                stack.push(lastSyntaxNode);
                break;
              end;
              if (stack.peek(0) is TdomXPathIsEqualOperator) and
                 ( (stack.peek(1) is TdomXPathIsEqualExpr) or
                   (stack.peek(1) is TdomXPathIsNotEqualExpr) )
              then begin
                // XPath 1.0, prod. [23]:
                stack.pop.free;
                newSyntaxNode:= TdomXPathIsEqualExpr.create(self, ''); // Create IsEqualExpr.
                newSyntaxNode.left:= stack.pop;                        // Append EqualityExpr.
                newSyntaxNode.right:= lastSyntaxNode;                  // Append RelationalExpr.
                lastSyntaxNode:= newSyntaxNode;
              end else if (stack.peek(0) is TdomXPathIsNotEqualOperator) and
                 ( (stack.peek(1) is TdomXPathIsEqualExpr) or
                   (stack.peek(1) is TdomXPathIsNotEqualExpr) )
              then begin
                // XPath 1.0, prod. [23]:
                stack.pop.free;
                newSyntaxNode:= TdomXPathIsNotEqualExpr.create(self, ''); // Create IsNotEqualExpr.
                newSyntaxNode.left:= stack.pop;                           // Append EqualityExpr.
                newSyntaxNode.right:= lastSyntaxNode;                     // Append RelationalExpr.
                lastSyntaxNode:= newSyntaxNode;
              end else begin
                // XPath 1.0, prod. [23]:
                newSyntaxNode:= TdomXPathIsEqualExpr.create(self, ''); // Create IsEqualExpr.
                newSyntaxNode.left:= lastSyntaxNode;                   // Append RelationalExpr.
                lastSyntaxNode:= newSyntaxNode;
              end;
            end else if (lastSyntaxNode is TdomXPathGreaterThanOperator) or
                        (lastSyntaxNode is TdomXPathGreaterThanOrEqualOperator)
            then begin
              stack.push(lastSyntaxNode);
              break;
            end else if (lastSyntaxNode is TdomXPathIsEqualExpr) or
                        (lastSyntaxNode is TdomXPathIsNotEqualExpr)
            then begin
              if tokenizer.isFollowing(XPATH_SHEFFER_STROKE_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_MULTIPLY_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_DIV_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_MOD_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_PLUS_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_MINUS_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_LESS_THAN_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_LESS_THAN_OR_EQUAL_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_GREATER_THAN_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_GREATER_THAN_OR_EQUAL_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_IS_EQUAL_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_IS_NOT_EQUAL_OPERATOR_TOKEN)
              then begin
                // Operator of higher precedence is following, so we postpone building the expression.
                stack.push(lastSyntaxNode);
                break;
              end;
              if (stack.peek(0) is TdomXPathAndOperator) and
                 ( (stack.peek(1) is TdomXPathAndExpr) )
              then begin
                // XPath 1.0, prod. [22]:
                stack.pop.free;
                newSyntaxNode:= TdomXPathAndExpr.create(self, ''); // Create AndExpr.
                newSyntaxNode.left:= stack.pop;                    // Append AndExpr.
                newSyntaxNode.right:= lastSyntaxNode;              // Append EqualityExpr.
                lastSyntaxNode:= newSyntaxNode;
              end else begin
                // XPath 1.0, prod. [22]:
                newSyntaxNode:= TdomXPathAndExpr.create(self, ''); // Create AndExpr.
                newSyntaxNode.left:= lastSyntaxNode;               // Append EqualityExpr.
                lastSyntaxNode:= newSyntaxNode;
              end;
            end else if (lastSyntaxNode is TdomXPathIsEqualOperator) or
                        (lastSyntaxNode is TdomXPathIsNotEqualOperator) or
                        (lastSyntaxNode is TdomXPathLeftParenthesis) or
                        (lastSyntaxNode is TdomXPathLeftSquareBracket) or
                        (lastSyntaxNode is TdomXPathLessThanOperator) or
                        (lastSyntaxNode is TdomXPathLessThanOrEqualOperator)
            then begin
              stack.push(lastSyntaxNode);
              break;
            end else if lastSyntaxNode is TdomXPathLiteral then begin
              if (stack.peek(0) is TdomXPathLeftParenthesis) and
                 (stack.peek(1) is TdomXPathNodeTypePI) and
                 tokenizer.isFollowing(XPATH_RIGHT_PARENTHESIS_TOKEN)
              then begin
                // Literal is part of a processing-instruction node test,
                // so we postpone building the expression.
                stack.push(lastSyntaxNode);
                break;
              end else begin
                // XPath 1.0, prod. [15]:
                newSyntaxNode:= TdomXPathPrimaryExpr.create(self, ''); // Create PrimaryExpr.
                newSyntaxNode.left:= lastSyntaxNode;                   // Append Literal.
                lastSyntaxNode:= newSyntaxNode;
              end;
            end else if (lastSyntaxNode is TdomXPathMinusExpr) or
                        (lastSyntaxNode is TdomXPathPlusExpr)
            then begin
              if tokenizer.isFollowing(XPATH_SHEFFER_STROKE_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_MULTIPLY_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_DIV_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_MOD_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_PLUS_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_MINUS_OPERATOR_TOKEN)
              then begin
                // Operator of higher precedence is following, so we postpone building the expression.
                stack.push(lastSyntaxNode);
                break;
              end;
              if (stack.peek(0) is TdomXPathLessThanOperator) and
                 ( (stack.peek(1) is TdomXPathLessThanExpr) or
                   (stack.peek(1) is TdomXPathLessThanOrEqualExpr) or
                   (stack.peek(1) is TdomXPathGreaterThanExpr) or
                   (stack.peek(1) is TdomXPathGreaterThanOrEqualExpr) )
              then begin
                // XPath 1.0, prod. [24]:
                stack.pop.free;
                newSyntaxNode:= TdomXPathLessThanExpr.create(self, ''); // Create LessThanExpr.
                newSyntaxNode.left:= stack.pop;                         // Append RelationalExpr.
                newSyntaxNode.right:= lastSyntaxNode;                   // Append AdditiveExpr.
                lastSyntaxNode:= newSyntaxNode;
              end else if (stack.peek(0) is TdomXPathLessThanOrEqualOperator) and
                 ( (stack.peek(1) is TdomXPathLessThanExpr) or
                   (stack.peek(1) is TdomXPathLessThanOrEqualExpr) or
                   (stack.peek(1) is TdomXPathGreaterThanExpr) or
                   (stack.peek(1) is TdomXPathGreaterThanOrEqualExpr) )
              then begin
                // XPath 1.0, prod. [24]:
                stack.pop.free;
                newSyntaxNode:= TdomXPathLessThanOrEqualExpr.create(self, ''); // Create LessThanOrEqualExpr.
                newSyntaxNode.left:= stack.pop;                                // Append RelationalExpr.
                newSyntaxNode.right:= lastSyntaxNode;                          // Append AdditiveExpr.
                lastSyntaxNode:= newSyntaxNode;
              end else if (stack.peek(0) is TdomXPathGreaterThanOperator) and
                 ( (stack.peek(1) is TdomXPathLessThanExpr) or
                   (stack.peek(1) is TdomXPathLessThanOrEqualExpr) or
                   (stack.peek(1) is TdomXPathGreaterThanExpr) or
                   (stack.peek(1) is TdomXPathGreaterThanOrEqualExpr) )
              then begin
                // XPath 1.0, prod. [24]:
                stack.pop.free;
                newSyntaxNode:= TdomXPathGreaterThanExpr.create(self, ''); // Create GreaterThanExpr.
                newSyntaxNode.left:= stack.pop;                            // Append RelationalExpr.
                newSyntaxNode.right:= lastSyntaxNode;                      // Append AdditiveExpr.
                lastSyntaxNode:= newSyntaxNode;
              end else if (stack.peek(0) is TdomXPathGreaterThanOrEqualOperator) and
                 ( (stack.peek(1) is TdomXPathLessThanExpr) or
                   (stack.peek(1) is TdomXPathLessThanOrEqualExpr) or
                   (stack.peek(1) is TdomXPathGreaterThanExpr) or
                   (stack.peek(1) is TdomXPathGreaterThanOrEqualExpr) )
              then begin
                // XPath 1.0, prod. [24]:
                stack.pop.free;
                newSyntaxNode:= TdomXPathGreaterThanOrEqualExpr.create(self, ''); // Create GreaterThanOrEqualExpr.
                newSyntaxNode.left:= stack.pop;                                   // Append RelationalExpr.
                newSyntaxNode.right:= lastSyntaxNode;                             // Append AdditiveExpr.
                lastSyntaxNode:= newSyntaxNode;
              end else begin
                // XPath 1.0, prod. [24]:
                newSyntaxNode:= TdomXPathLessThanExpr.create(self, ''); // Create LessThanExpr.
                newSyntaxNode.left:= lastSyntaxNode;                    // Append AdditiveExpr.
                lastSyntaxNode:= newSyntaxNode;
              end;
            end else if (lastSyntaxNode is TdomXPathMinusOperator) or
                        (lastSyntaxNode is TdomXPathModOperator) or
                        (lastSyntaxNode is TdomXPathMultiplyOperator)
            then begin
              stack.push(lastSyntaxNode);
              break;
            end else if lastSyntaxNode is TdomXPathNameTest then begin
              // XPath 1.0, prod. [7]:
              newSyntaxNode:= TdomXPathNodeTest.create(self, '');  // Create NodeTest.
              newSyntaxNode.left:= lastSyntaxNode;                 // Append NameTest.
              lastSyntaxNode:= newSyntaxNode;
            end else if lastSyntaxNode is TdomXPathNodeTest then begin
              // XPath 1.0, prod. [4]:
              if tokenizer.isFollowing(XPATH_LEFT_SQUARE_BRACKET_TOKEN) then begin
                // A Predicate is following, so we postpone building the Step.
                stack.push(lastSyntaxNode);
                break;
              end;
              if stack.peek(0) is TdomXPathDoubleColon then begin
                if stack.peek(1) is TdomXPathCustomAxisName then begin
                  stack.pop.free;
                  newSyntaxNode:= stack.pop;
                  newSyntaxNode.left:= lastSyntaxNode;             // Append NodeTest to AxisName.
                  lastSyntaxNode:= TdomXPathStep.create(self, ''); // Create Step.
                  lastSyntaxNode.left:= newSyntaxNode;             // Append AxisName to Step.
                end else begin
                  // Malformed XPath Expression.  We are parsing it anyway ...
                  stack.push(lastSyntaxNode);
                  break;
                end;
              end else if stack.peek(0) is TdomXPathCommercialAt then begin
                // XPath 1.0, prod. [13]:
                stack.pop.free;
                newSyntaxNode:= TdomXPathAxisNameAttribute.create(self, '');
                newSyntaxNode.left:= lastSyntaxNode;             // Append NodeTest to AxisName.
                lastSyntaxNode:= TdomXPathStep.create(self, ''); // Create Step.
                lastSyntaxNode.left:= newSyntaxNode;             // Append AxisName to Step.
              end else begin
                // XPath 1.0, prod. [13]:
                newSyntaxNode:= TdomXPathAxisNameChild.create(self, '');
                newSyntaxNode.left:= lastSyntaxNode;             // Append NodeTest to AxisName.
                lastSyntaxNode:= TdomXPathStep.create(self, ''); // Create Step.
                lastSyntaxNode.left:= newSyntaxNode;             // Append AxisName to Step.
              end;
            end else if (lastSyntaxNode is TdomXPathNodeTypeComment) or
                        (lastSyntaxNode is TdomXPathNodeTypeNode) or
                        (lastSyntaxNode is TdomXPathNodeTypePI) or
                        (lastSyntaxNode is TdomXPathNodeTypeText)
            then begin
              stack.push(lastSyntaxNode);
              break;
            end else if lastSyntaxNode is TdomXPathNumber then begin
              // XPath 1.0, prod. [15]:
              newSyntaxNode:= TdomXPathPrimaryExpr.create(self, ''); // Create PrimaryExpr.
              newSyntaxNode.left:= lastSyntaxNode;                   // Append Number.
              lastSyntaxNode:= newSyntaxNode;
            end else if lastSyntaxNode is TdomXPathOrExpr then begin
              if tokenizer.isFollowing(XPATH_SHEFFER_STROKE_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_MULTIPLY_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_DIV_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_MOD_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_PLUS_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_MINUS_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_LESS_THAN_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_LESS_THAN_OR_EQUAL_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_GREATER_THAN_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_GREATER_THAN_OR_EQUAL_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_IS_EQUAL_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_IS_NOT_EQUAL_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_AND_OPERATOR_TOKEN) or
                 tokenizer.isFollowing(XPATH_OR_OPERATOR_TOKEN)
              then begin
                // Operator of higher precedence is following, so we postpone building the expression.
                stack.push(lastSyntaxNode);
                break;
              end;
              // XPath 1.0, prod. [14]:
              newSyntaxNode:= TdomXPathExpr.create(self, ''); // Create Expr.
              newSyntaxNode.left:= lastSyntaxNode;            // Append OrExpr.
              lastSyntaxNode:= newSyntaxNode;
            end else if lastSyntaxNode is TdomXPathOrOperator then begin
              stack.push(lastSyntaxNode);
              break;
            end else if lastSyntaxNode is TdomXPathPathExpr then begin
              // XPath 1.0, prod. [18]:
              if tokenizer.isFollowing(XPATH_SLASH_OPERATOR_TOKEN) then begin
                // A Slash is following, so we postpone building the TdomXPathUnionExpr.
                stack.push(lastSyntaxNode);
                break;
              end;
              if (stack.peek(0) is TdomXPathShefferStrokeOperator) and
                 (stack.peek(1) is TdomXPathUnionExpr)
              then begin
                stack.pop.free;
                newSyntaxNode:= TdomXPathUnionExpr.create(self, ''); // Create UnionExpr.
                newSyntaxNode.left:= stack.pop;                      // Append UnionExpr.
                newSyntaxNode.right:= lastSyntaxNode;                // Append PathExpr.
                lastSyntaxNode:= newSyntaxNode;
              end else begin
                newSyntaxNode:= TdomXPathUnionExpr.create(self, ''); // Create UnionExpr.
                newSyntaxNode.left:= lastSyntaxNode;                 // Append PathExpr.
                lastSyntaxNode:= newSyntaxNode;
              end;
            end else if lastSyntaxNode is TdomXPathPlusOperator then begin
              stack.push(lastSyntaxNode);
              break;
            end else if lastSyntaxNode is TdomXPathPredicate then begin
              if stack.peek(0) is TdomXPathFilterExpr then begin
                // XPath 1.0, prod. [20]:
                newSyntaxNode:= TdomXPathFilterExpr.create(self, '');
                newSyntaxNode.left:= stack.pop;
                newSyntaxNode.right:= lastSyntaxNode;
                lastSyntaxNode:= newSyntaxNode;
              end else begin
                // XPath 1.0, prod. [4]:
                if tokenizer.isFollowing(XPATH_LEFT_SQUARE_BRACKET_TOKEN) then begin
                  // Another Predicate is following, so we postpone building the Step.
                  stack.push(lastSyntaxNode);
                  break;
                end;
                if stack.peek(0) is TdomXPathPredicate then begin
                  newSyntaxNode:= stack.pop;
                  newSyntaxNode.right:= lastSyntaxNode;
                  lastSyntaxNode:= newSyntaxNode;
                end else if stack.peek(0) is TdomXPathNodeTest then begin
                  if stack.peek(1) is TdomXPathDoubleColon then begin
                    if stack.peek(2) is TdomXPathCustomAxisName then begin
                      nodeTestNode:= stack.pop; // Pop the NodeTest from the stack.
                      stack.pop.free;           // Pop and delete the DoubleColon.
                      axisNode:= stack.pop;     // Pop the AxisName from the stack.
                      axisNode.left:= nodeTestNode;                    // Append NodeTest to AxisName.
                      axisNode.right:= lastSyntaxNode;                 // Append Predicate to AxisName.
                      lastSyntaxNode:= TdomXPathStep.create(self, ''); // Create Step.
                      lastSyntaxNode.left:= axisNode;                  // Append AxisName to Step.
                    end else begin
                      // Malformed XPath Expression.  We are parsing it anyway ...
                      stack.push(lastSyntaxNode);
                      break;
                    end;
                  end else if stack.peek(1) is TdomXPathCommercialAt then begin
                    // XPath 1.0, prod. [13]:
                    nodeTestNode:= stack.pop;                               // Pop the NodeTest from the stack.
                    stack.pop.free;                                         // Pop and delete the DoubleColon.
                    axisNode:= TdomXPathAxisNameAttribute.create(self, ''); // Create attribute axis AxisName.
                    axisNode.left:= nodeTestNode;                           // Append NodeTest to AxisName.
                    axisNode.right:= lastSyntaxNode;                        // Append Predicate to AxisName.
                    lastSyntaxNode:= TdomXPathStep.create(self, '');        // Create Step.
                    lastSyntaxNode.left:= axisNode;                         // Append AxisName to Step.
                  end else begin
                    // XPath 1.0, prod. [13]:
                    nodeTestNode:= stack.pop;                            // Pop the NodeTest from the stack.
                    axisNode:= TdomXPathAxisNameChild.create(self, '');  // Create child axis AxisName.
                    axisNode.left:= nodeTestNode;                        // Append NodeTest to AxisName.
                    axisNode.right:= lastSyntaxNode;                     // Append Predicate to AxisName.
                    lastSyntaxNode:= TdomXPathStep.create(self, '');     // Create Step.
                    lastSyntaxNode.left:= axisNode;                      // Append AxisName to Step.
                  end;
                end else begin
                  // Malformed XPath Expression.  We are parsing it anyway ...
                  stack.push(lastSyntaxNode);
                  break;
                end;
              end;
            end else if lastSyntaxNode is TdomXPathPrimaryExpr then begin
              // XPath 1.0, prod. [20]:
              newSyntaxNode:= TdomXPathFilterExpr.create(self, ''); // Create FilterExpr.
              newSyntaxNode.left:= lastSyntaxNode;                  // Append PrimaryExpr.
              lastSyntaxNode:= newSyntaxNode;
            end else if lastSyntaxNode is TdomXPathRightParenthesis then begin
              // XPath 1.0, prod. [7]:
              if (stack.peek(0) is TdomXPathLeftParenthesis) and
                 ( (stack.peek(1) is TdomXPathNodeTypeComment) or
                   (stack.peek(1) is TdomXPathNodeTypeNode) or
                   (stack.peek(1) is TdomXPathNodeTypePI) or
                   (stack.peek(1) is TdomXPathNodeTypeText) )
              then begin
                lastSyntaxNode.free;
                lastSyntaxNode:= TdomXPathNodeTest.create(self, '');
                stack.pop.free;
                lastSyntaxNode.left:= stack.pop;
              end else if (stack.peek(0) is TdomXPathLiteral) and
                          (stack.peek(1) is TdomXPathLeftParenthesis) and
                          (stack.peek(2) is TdomXPathNodeTypePI)
              then begin
                lastSyntaxNode.free;
                lastSyntaxNode:= TdomXPathNodeTest.create(self, ''); // Create NodeTest
                PILiteral:= stack.pop;
                stack.pop.free;                                      // Remove LeftParenthesist from stack.
                nodeTypePI:= stack.pop;
                nodeTypePI.left:= PILiteral;                         // Append Literal to NodeTypePI
                lastSyntaxNode.left:= nodeTypePI;                    // Append NodeTypePI to NodeTest
              end else if (stack.peek(0) is TdomXPathExpr) and
                          (stack.peek(1) is TdomXPathLeftParenthesis) and not
                          (stack.peek(2) is TdomXPathFunctionName)
              then begin
                lastSyntaxNode.free;
                newSyntaxNode:= TdomXPathPrimaryExpr.create(self, ''); // Create PrimaryExpr.
                newSyntaxNode.left:= stack.pop;                        // Append Expr.
                stack.pop.free;                                        // Remove LeftParenthesis from stack.
                lastSyntaxNode:= newSyntaxNode;
              end else begin
                // XPath 1.0, prod. [16]:
                functionCallNode:= TdomXPathFunctionCall.create(self, ''); // Create FunctionCall.
                while stack.peek(0) is TdomXPathExpr do begin
                  functionCallNode.arguments.Insert(0, stack.pop); // Add Expr as first argument to FunctionCall.
                  if stack.peek(0) is TdomXPathComma then begin
                    stack.pop.free                                 // Remove Comma from stack.
                  end else if not (stack.peek(0) is TdomXPathLeftParenthesis) then begin
                    // Malformed XPath Expression.  We are parsing it anyway ...
                    break;
                  end;
                end;
                if (stack.peek(0) is TdomXPathLeftParenthesis) and
                   (stack.peek(1) is TdomXPathFunctionName)
                then begin
                  // XPath 1.0, prod. [15]:
                  lastSyntaxNode.free;
                  stack.pop.free;                                        // Remove LeftParenthesis from stack.
                  functionCallNode.functionName := stack.peek(0).value;  // Set function name on FunctionCallNode.
                  stack.pop.free;                                        // Remove FunctionName.
                  newSyntaxNode:= TdomXPathPrimaryExpr.create(self, ''); // Create PrimaryExpr.
                  newSyntaxNode.left:= functionCallNode;                 // Append FunctionCall.
                  lastSyntaxNode:= newSyntaxNode;
                end else begin
                  // Malformed XPath Expression.  We are parsing it anyway ...
                  stack.push(functionCallNode);
                  stack.push(lastSyntaxNode);
                  break;
                end;
              end;
            end else if lastSyntaxNode is TdomXPathRightSquareBracket then begin
              // XPath 1.0, prod. [8] and [9]:
              if (stack.peek(0) is TdomXPathExpr) and
                 (stack.peek(1) is TdomXPathLeftSquareBracket)
              then begin
                lastSyntaxNode.free;
                lastSyntaxNode:= TdomXPathPredicate.create(self, ''); // Create Predicate.
                lastSyntaxNode.left:= stack.pop;                      // Append Expr.
                stack.pop.free;                                       // Remove LeftSquareBracket from stack.
              end else begin
                // Malformed XPath Expression.  We are parsing it anyway ...
                stack.push(lastSyntaxNode);
                break;
              end;
            end else if lastSyntaxNode is TdomXPathShefferStrokeOperator then begin
              if stack.peek(0) is TdomXPathPathExpr then begin
                newSyntaxNode:= TdomXPathUnionExpr.create(self, ''); // Create UnionExpr.
                newSyntaxNode.left:= stack.pop;                      // Append PathExpr from stack.
                stack.push(newSyntaxNode);                           // Push the UnionExpr on the stack.
                stack.push(lastSyntaxNode);                          // Push the ShefferStrokeOperator on the stack.
                break;
              end else begin
                // Malformed XPath Expression.  We are parsing it anyway ...
                stack.push(lastSyntaxNode);
                break;
              end;
            end else if lastSyntaxNode is TdomXPathSingleDot then begin
              // XPath 1.0, prod. [12]:
              lastSyntaxNode.free;
              lastSyntaxNode:= TdomXPathStep.create(self, '');                        // Create Step.
              lastSyntaxNode.left:= TdomXPathAxisNameSelf.create(self, '');           // Create and append AxisName to Step.
              lastSyntaxNode.left.left:= TdomXPathNodeTest.create(self, '');          // Create and append NodeTest to AxisName.
              lastSyntaxNode.left.left.left:= TdomXPathNodeTypeNode.create(self, ''); // Create and append NodeType to NodeTest.
            end else if lastSyntaxNode is TdomXPathSlashOperator then begin
              // XPath 1.0, prod. [2]:
              if ( (not assigned(stack.peek(0))) or
                   (stack.peek(0) is TdomXPathShefferStrokeOperator) ) and
                 ( tokenizer.isFollowing(XPATH_END_OF_TEXT_TOKEN) or
                   tokenizer.isFollowing(XPATH_SHEFFER_STROKE_OPERATOR_TOKEN)    )
              then begin
                lastSyntaxNode.free;
                lastSyntaxNode:= TdomXPathPathExpr.create(self, '');                  // Create PathExpr.
                lastSyntaxNode.left:= TdomXPathAbsoluteLocationPath.create(self, ''); // Create and append AbsolutLocationPath.
              end else begin
                stack.push(lastSyntaxNode);
                break;
              end;
            end else if lastSyntaxNode is TdomXPathStep then begin
              // XPath 1.0, prod. [3] and [19]:
              if stack.peek(0) is TdomXPathSlashOperator then begin
                if stack.peek(1) is TdomXPathFilterExpr then begin
                  stack.pop.free;
                  newSyntaxNode:= TdomXPathPathExpr.create(self, ''); // Create PathExpr.
                  newSyntaxNode.left:= stack.pop;                     // Append FilterExpr to PathExpr.
                  newSyntaxNode.right:= lastSyntaxNode;               // Append Step to PathExpr.
                  lastSyntaxNode:= newSyntaxNode;
                end else if stack.peek(1) is TdomXPathPathExpr then begin
                  stack.pop.free;
                  if TdomXPathPathExpr(stack.peek(0)).addStep(TdomXPathStep(lastSyntaxNode)) then begin
                    lastSyntaxNode:= stack.pop;
                  end else begin
                    // Malformed XPath Expression.  We are parsing it anyway ...
                    stack.push(lastSyntaxNode);
                    break;
                  end;
                end else if (not assigned(stack.peek(1)) ) or
                            (stack.peek(1) is TdomXPathShefferStrokeOperator)
                then begin
                  // XPath 1.0, prod. [2]:
                  stack.pop.free;
                  newSyntaxNode:= TdomXPathPathExpr.create(self, '');                  // Create PathExpr.
                  newSyntaxNode.left:= TdomXPathAbsoluteLocationPath.create(self, ''); // Create and append AbsolutLocationPath.
                  newSyntaxNode.right:= lastSyntaxNode;                                // Append Step.
                  lastSyntaxNode:= newSyntaxNode;
                end else begin
                  // Malformed XPath Expression.  We are parsing it anyway ...
                  stack.push(lastSyntaxNode);
                  break;
                end;
              end else begin
                newSyntaxNode:= TdomXPathPathExpr.create(self, ''); // Create PathExpr.
                newSyntaxNode.right:= lastSyntaxNode;               // Append Step to PathExpr.
                lastSyntaxNode:= newSyntaxNode;
              end;
            end else if lastSyntaxNode is TdomXPathUnaryExpr then begin
              if tokenizer.isFollowing(XPATH_SHEFFER_STROKE_OPERATOR_TOKEN) then begin
                // Operator of higher precedence is following, so we postpone building the expression.
                stack.push(lastSyntaxNode);
                break;
              end;
              if (stack.peek(0) is TdomXPathMinusOperator) and not (
                   (stack.peek(1) is TdomXPathPlusExpr) or
                   (stack.peek(1) is TdomXPathMinusExpr) or
                   (stack.peek(1) is TdomXPathMultiplyExpr) or
                   (stack.peek(1) is TdomXPathDivExpr) or
                   (stack.peek(1) is TdomXPathModExpr) or
                   (stack.peek(1) is TdomXPathUnaryExpr) or
                   (stack.peek(1) is TdomXPathUnionExpr)  )
              then begin
                // XPath 1.0, prod. [27]:
                newSyntaxNode:= TdomXPathUnaryExpr.create(self, ''); // Create UnaryExpr.
                newSyntaxNode.left:= stack.pop;                      // Append MinusOperator.
                newSyntaxNode.right:= lastSyntaxNode;                // Append UnaryExpr.
                lastSyntaxNode:= newSyntaxNode;
              end else if (stack.peek(0) is TdomXPathMultiplyOperator) and
                 ( (stack.peek(1) is TdomXPathMultiplyExpr) or
                   (stack.peek(1) is TdomXPathDivExpr) or
                   (stack.peek(1) is TdomXPathModExpr) )
              then begin
                // XPath 1.0, prod. [26]:
                stack.pop.free;
                newSyntaxNode:= TdomXPathMultiplyExpr.create(self, ''); // Create MultiplyExpr.
                newSyntaxNode.left:= stack.pop;                         // Append MultiplicativeExpr.
                newSyntaxNode.right:= lastSyntaxNode;                   // Append UnaryExpr.
                lastSyntaxNode:= newSyntaxNode;
              end else if (stack.peek(0) is TdomXPathDivOperator) and
                 ( (stack.peek(1) is TdomXPathMultiplyExpr) or
                   (stack.peek(1) is TdomXPathDivExpr) or
                   (stack.peek(1) is TdomXPathModExpr) )
              then begin
                // XPath 1.0, prod. [26]:
                stack.pop.free;
                newSyntaxNode:= TdomXPathDivExpr.create(self, ''); // Create DivExpr.
                newSyntaxNode.left:= stack.pop;                    // Append MultiplicativeExpr.
                newSyntaxNode.right:= lastSyntaxNode;              // Append UnaryExpr.
                lastSyntaxNode:= newSyntaxNode;
              end else if (stack.peek(0) is TdomXPathModOperator) and
                 ( (stack.peek(1) is TdomXPathMultiplyExpr) or
                   (stack.peek(1) is TdomXPathDivExpr) or
                   (stack.peek(1) is TdomXPathModExpr) )
              then begin
                // XPath 1.0, prod. [26]:
                stack.pop.free;
                newSyntaxNode:= TdomXPathModExpr.create(self, ''); // Create ModExpr.
                newSyntaxNode.left:= stack.pop;                    // Append MultiplicativeExpr.
                newSyntaxNode.right:= lastSyntaxNode;              // Append UnaryExpr.
                lastSyntaxNode:= newSyntaxNode;
              end else begin
                // XPath 1.0, prod. [26]:
                newSyntaxNode:= TdomXPathMultiplyExpr.create(self, ''); // Create MultiplyExpr.
                newSyntaxNode.left:= lastSyntaxNode;                    // Append UnaryExpr.
                lastSyntaxNode:= newSyntaxNode;
              end;
            end else if lastSyntaxNode is TdomXPathUnionExpr then begin
              // XPath 1.0, prod. [27]:
              if tokenizer.isFollowing(XPATH_SHEFFER_STROKE_OPERATOR_TOKEN) then begin
                // A Sheffer's Stroke is following, so we postpone building the UnaryExpr.
                stack.push(lastSyntaxNode);
                break;
              end;
              newSyntaxNode:= TdomXPathUnaryExpr.create(self, ''); // Create UnaryExpr.
              newSyntaxNode.left:= lastSyntaxNode;                 // Append the UnionExpr.
              lastSyntaxNode:= newSyntaxNode;
            end else if lastSyntaxNode is TdomXPathVariableReference then begin
              // XPath 1.0, prod. [15]:
              newSyntaxNode:= TdomXPathPrimaryExpr.create(self, ''); // Create PrimaryExpr.
              newSyntaxNode.left:= lastSyntaxNode;                   // Append VariableReference.
              lastSyntaxNode:= newSyntaxNode;
            end;
          until false;
        end; {case ... else ...}
      until false;

      // Is the syntax tree valid, i.e. does the evaluation reach the end of the text
      // and does the stack hold exactly one root node of type TdomXPathExpr?
      if (symbol =  XPATH_END_OF_TEXT_TOKEN) and
         (stack.length = 1) and
         (stack.peek(0) is TdomXPathExpr)
      then begin
        FRootExpr:= TdomXPathExpr(stack.pop);
        result:= true;
      end else result:= false;

    finally
      stack.free; // Remark: Frees also all object still in the stack.
    end;
  finally
    tokenizer.free;
  end;
end;

function TdomXPathSyntaxTree.getContextNode: TdomNode;
begin
  if assigned(ownerXPathExpression)
    then result:= ownerXPathExpression.contextNode
    else result:= nil;
end;

{ TXPathExpression }

constructor TXPathExpression.create(aOwner: TComponent);
begin
  inherited;
  FIsValid:= T_UNKNOWN;
  FSyntaxTree:= TdomXPathSyntaxTree.create(self);
end;

destructor TXPathExpression.destroy;
begin
  FXPathResult.Free;
  FSyntaxTree.Free;
  inherited;
end;

function TXPathExpression.acquireXPathResult(const resultType: TdomXPathResultClass): TdomXPathCustomResult;
begin
  if resultType = TdomXPathNodeSetResult then begin
    result:= TdomXPathNodeSetResult.create;
    result.Assign(self);
  end else if resultType = TdomXPathBooleanResult then begin
    result:= TdomXPathBooleanResult.create(resultAsBoolean);
  end else if resultType = TdomXPathNumberResult then begin
    result:= TdomXPathNumberResult.create(resultAsNumber);
  end else if resultType = TdomXPathStringResult then begin
    result:= TdomXPathStringResult.create(resultAsWideString);
  end else raise ENot_Supported_Err.create('Not supported error.');
end;

function TXPathExpression.evaluate: boolean;
begin
  FXPathResult.Free;
  FXPathResult:= nil;
  try
    result:= prepare;
    if result then
      FXPathResult:= FSyntaxTree.evaluate;
  except
    result:= false;
    FXPathResult.Free;
    FXPathResult:= nil;
  end;
end;

function TXPathExpression.hasNodeSetResult: boolean;
begin
  if assigned(FXPathResult)
    then result := FXPathResult.length > 0
    else result := false;
end;

function TXPathExpression.prepare: boolean;
begin
  if isValid = T_UNKNOWN then begin
    result:= FSyntaxTree.prepare(FExpression);
    if result
      then FIsValid:= T_TRUE
      else FIsValid:= T_FALSE;
  end else result:= FIsValid = T_TRUE;
end;

function TXPathExpression.resultAsBoolean: boolean;
begin
  if assigned(FXPathResult)
    then result := FXPathResult.asBoolean
    else result := false;
end;

function TXPathExpression.resultAsNumber: double;
begin
  if assigned(FXPathResult)
    then result := FXPathResult.asNumber
    else result := NaN;
end;

function TXPathExpression.resultAsWideString: wideString;
begin
  if assigned(FXPathResult)
    then result := FXPathResult.asWideString
    else result := '';
end;

function TXPathExpression.resultAxisType: TdomXPathAxisType;
begin
  if assigned(FXPathResult)
    then result := FXPathResult.axisType
    else result := XPATH_FORWARD_AXIS;
end;

function TXPathExpression.resultLength: integer;
begin
  if assigned(FXPathResult)
    then result := FXPathResult.length
    else result := 0;
end;

function TXPathExpression.resultNode(const index: integer): TdomNode;
begin
  if assigned(FXPathResult)
    then result := FXPathResult.item(index)
    else result := nil;
end;

procedure TXPathExpression.setContextNode(const node: TdomNode);
begin
  if assigned(node) then
    if not (node.nodeType in [ ntElement_Node,
                               ntAttribute_Node,
                               ntText_Node,
                               ntProcessing_Instruction_Node,
                               ntComment_Node,
                               ntDocument_Node,
                               ntXPath_Namespace_Node ] ) then
      raise ENot_Supported_Err.create('Not supported error.');

  FContextNode := node;
end;

procedure TXPathExpression.setExpression(const S: wideString);
begin
  if S <> FExpression then begin
    FExpression:= S;
    FSyntaxTree.clear;
    FIsValid:= T_UNKNOWN;
  end;
end;

{ TdomXPathSyntaxNodeStack }

constructor TdomXPathSyntaxNodeStack.create;
begin
  inherited;
  FNodeList:= TList.create;
end;

destructor TdomXPathSyntaxNodeStack.destroy;
begin
  clear;
  FNodeList.free;
  inherited;
end;

procedure TdomXPathSyntaxNodeStack.clear;
var
  i: integer;
begin
  for i:= 0 to pred(FNodeList.Count) do
    TdomXPathSyntaxNode(FNodeList[i]).free;
end;

function TdomXPathSyntaxNodeStack.getLength: integer;
begin
  result:= FNodeList.count;
end;

function TdomXPathSyntaxNodeStack.peek(offset: integer): TdomXPathSyntaxNode;
var
  index: integer;
begin
  index:= pred(FNodeList.count)-offset;
  if (index < 0) or (index >= FNodeList.count)
    then result:= nil
    else result:= TdomXPathSyntaxNode(FNodeList.List^[index]);
end;

function TdomXPathSyntaxNodeStack.pop: TdomXPathSyntaxNode;
begin
  result:= FNodeList[pred(FNodeList.count)];
  FNodeList.Delete(pred(FNodeList.count));
end;

function TdomXPathSyntaxNodeStack.push(node: TdomXPathSyntaxNode): TdomXPathSyntaxNode;
begin
  result:= TdomXPathSyntaxNode(FNodeList.add(node));
end;

{ TdomXPathSyntaxNode }

constructor TdomXPathSyntaxNode.create(const AOwner: TdomXPathSyntaxTree;
                                       const value: wideString);
begin
  inherited create(AOwner);
  FLeft:= nil;
  FRight:= nil;
  FValue:= value;
end;

function TdomXPathSyntaxNode.getOwnerSyntaxTree: TdomXPathSyntaxTree;
begin
  Result:= (GetOwner as TdomXPathSyntaxTree);
end;

{ TdomXPathStep }

function TdomXPathStep.addStep(const step: TdomXPathStep): boolean;
begin
  if not assigned(right) then begin
    right:= step;
    result:= true;
  end else begin
    if right is TdomXPathStep
      then result:= TdomXPathStep(right).addStep(step)
      else result:= false;
  end;
end;

function TdomXPathStep.evaluate(const oldSnapshotResult: TdomXPathNodeSetResult): TdomXPathNodeSetResult;
var
  newResult: TdomXPathNodeSetResult;
begin
  if not assigned(oldSnapshotResult)
    then raise EXPath_Type_Err.create('XPath type error.');
  if left is TdomXPathCustomAxisName then begin

    if oldSnapshotResult.length > 0 then begin
      newResult:= TdomXPathCustomAxisName(left).evaluate(oldSnapshotResult);
      if right is TdomXPathStep
        then result:= TdomXPathStep(right).evaluate(newResult)
        else result:= newResult;
    end else result:= oldSnapshotResult;

  end else raise EXPath_Type_Err.create('XPath type error.');
end;

{ TdomXPathCustomAxisName }

constructor TdomXPathCustomAxisName.create(const AOwner: TdomXPathSyntaxTree;
                                           const value: wideString);
begin
  inherited;
  FAxisType:= XPATH_FORWARD_AXIS;
  FPrincipalNodeType:= ntElement_Node;
end;

function TdomXPathCustomAxisName.evaluate(const oldSnapshotResult: TdomXPathNodeSetResult): TdomXPathNodeSetResult;
var
  i: integer;
  n: TdomNode;
  axisNodeSnapshot,inputSnapshot,nodeTestSnapshot: TdomXPathNodeSetResult;

  function evaluatePredicate(const snapshot: TdomXPathNodeSetResult):TdomXPathNodeSetResult;
  begin
    if assigned(right) then begin
      if right is TdomXPathPredicate then begin
        if snapshot.length > 0
          then result:= TdomXPathPredicate(right).evaluate(snapshot)
          else result:= snapshot;
      end else begin
        snapshot.free;
        raise EXPath_Type_Err.create('XPath type error.');
      end;
    end else result:= snapshot;
  end;

  function evaluateNodeTest(const snapshot: TdomXPathNodeSetResult):TdomXPathNodeSetResult;
  begin
    if assigned(left) then begin
      if left is TdomXPathNodeTest then begin
        if snapshot.length > 0
          then result:= TdomXPathNodeTest(left).evaluate(snapshot,FPrincipalNodeType)
          else result:= snapshot;
      end else begin
        snapshot.free;
        raise EXPath_Type_Err.create('XPath type error.');
      end;
    end else raise EXPath_Type_Err.create('XPath type error.');
  end;

begin
  if not assigned(oldSnapshotResult)
    then raise EXPath_Type_Err.create('XPath type error.');
  try
    result:= TdomXPathNodeSetResult.create;
    try
      result.axisType:= axisType;
      with oldSnapshotResult do begin
        for i:= 0 to pred(length) do begin
          n:= item(i);
          if assigned(n) then begin
            inputSnapshot:= getAxisNodeSnapshot(n);
            nodeTestSnapshot:= evaluateNodeTest(inputSnapshot);
            axisNodeSnapshot:= evaluatePredicate(nodeTestSnapshot);
            try
              result.merge(axisNodeSnapshot);
            finally
              axisNodeSnapshot.free;
            end;
          end;
        end;
      end;
    except
      result.free;
      raise;
    end;
  finally
    oldSnapshotResult.free;
  end;
end;

{ TdomXPathAxisNameAncestor }

constructor TdomXPathAxisNameAncestor.create(const AOwner: TdomXPathSyntaxTree;
                                             const value: wideString);
begin
  inherited;
  FAxisType:= XPATH_REVERSE_AXIS;
end;

function TdomXPathAxisNameAncestor.getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult;
var
  n: TdomNode;
begin
  result:= TdomXPathNodeSetResult.create;
  result.axisType:= axisType;
  if assigned(contextNode) then begin
    case contextNode.nodeType of
      ntElement_Node,ntText_Node,ntCDATA_Section_Node,ntEntity_Reference_Node,
      ntProcessing_Instruction_Node,ntComment_Node:
        n:= contextNode.parentNode;
      ntAttribute_Node:
        n:= TdomAttr(contextNode).ownerElement;
      ntXPath_Namespace_Node:
        n:= TdomXPathNamespace(contextNode).ownerElement;
    else
      n:= nil;
    end;
    while assigned(n) do begin
      result.add(n);
      n:= n.parentNode;
    end;
  end;
end;

{ TdomXPathAxisNameAncestorOrSelf }

constructor TdomXPathAxisNameAncestorOrSelf.create(const AOwner: TdomXPathSyntaxTree;
                                                   const value: wideString);
begin
  inherited;
  FAxisType:= XPATH_REVERSE_AXIS;
end;

function TdomXPathAxisNameAncestorOrSelf.getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult;
var
  n: TdomNode;
begin
  result:= TdomXPathNodeSetResult.create;
  result.axisType:= axisType;
  if assigned(contextNode) then begin
    result.add(contextNode);
    case contextNode.nodeType of
      ntElement_Node,ntText_Node,ntCDATA_Section_Node,ntEntity_Reference_Node,
      ntProcessing_Instruction_Node,ntComment_Node:
        n:= contextNode.parentNode;
      ntAttribute_Node:
        n:= TdomAttr(contextNode).ownerElement;
      ntXPath_Namespace_Node:
        n:= TdomXPathNamespace(contextNode).ownerElement;
    else
      n:= nil;
    end;
    while assigned(n) do begin
      result.add(n);
      n:= n.parentNode;
    end;
  end;
end;

{ TdomXPathAxisNameAttribute }

constructor TdomXPathAxisNameAttribute.create(const AOwner: TdomXPathSyntaxTree;
                                              const value: wideString);
begin
  inherited;
  FPrincipalNodeType:= ntAttribute_Node;
end;

function TdomXPathAxisNameAttribute.getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult;
var
  i: integer;
begin
  result:= TdomXPathNodeSetResult.create;
  result.axisType:= axisType;
  if assigned(contextNode) then
    with contextNode do
      if nodeType = ntElement_Node then
        with attributes do
          for i:= 0 to pred(length) do
            if (item(i) as TdomAttr).isXmlnsDecl = NSDT_NONE then
              result.add(item(i));
end;

{ TdomXPathAxisNameChild }

function TdomXPathAxisNameChild.getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult;
var
  i: integer;
begin
  result:= TdomXPathNodeSetResult.create;
  result.axisType:= axisType;
  if assigned(contextNode) then
    with contextNode.childNodes do
      for i:= 0 to pred(length) do
        result.add(item(i));
end;

{ TdomXPathAxisNameDescendant }

function TdomXPathAxisNameDescendant.getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult;
var
  n: TdomNode;
begin
  result:= TdomXPathNodeSetResult.create;
  result.axisType:= axisType;
  if assigned(contextNode) then begin
    with contextNode.referenceDocument.createNodeIterator(contextNode,
                                                      [ ntElement_Node,
                                                        ntText_Node,
                                                        ntCDATA_Section_Node,
                                                        ntEntity_Reference_Node,
                                                        ntProcessing_Instruction_Node,
                                                        ntComment_Node ],
                                                      nil,
                                                      false) do begin
      n:= NextNode;
      if n = contextNode then n:= NextNode;
      while assigned(n) do begin
        result.add(n);
        n:= NextNode;
      end;
      detach;
    end;
    contextNode.referenceDocument.clearInvalidNodeIterators;
  end;
end;

{ TdomXPathAxisNameDescendantOrSelf }

function TdomXPathAxisNameDescendantOrSelf.getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult;
var
  n: TdomNode;
begin
  result:= TdomXPathNodeSetResult.create;
  result.axisType:= axisType;
  if assigned(contextNode) then begin
    with contextNode.referenceDocument.createNodeIterator(contextNode,
                                                      [ ntElement_Node,
                                                        ntText_Node,
                                                        ntCDATA_Section_Node,
                                                        ntEntity_Reference_Node,
                                                        ntProcessing_Instruction_Node,
                                                        ntComment_Node,
                                                        ntDocument_Node ],
                                                      nil,
                                                      false) do begin
      n:= NextNode;
      while assigned(n) do begin
        result.add(n);
        n:= NextNode;
      end;
      detach;
    end;
    contextNode.referenceDocument.clearInvalidNodeIterators;
  end;
end;

{ TdomXPathAxisNameFollowing }

function TdomXPathAxisNameFollowing.getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult;
var
  p, q: TdomNode;
begin
  result:= TdomXPathNodeSetResult.create;
  result.axisType:= axisType;
  if assigned(contextNode) then begin
    case contextNode.nodeType of
      ntElement_Node,ntText_Node,ntCDATA_Section_Node,ntEntity_Reference_Node,
      ntProcessing_Instruction_Node,ntComment_Node: begin
        q:= contextNode;
        p:= contextNode.nextSibling;
        while assigned(p) do begin
          if not ( ( (q.nodeType = ntText_Node) or
                     (q.nodeType = ntCDATA_Section_Node) or
                     (q.nodeType = ntEntity_Reference_Node) ) and
                   ( (p.nodeType = ntText_Node) or
                     (p.nodeType = ntCDATA_Section_Node) or
                     (p.nodeType = ntEntity_Reference_Node) ) )
          then result.addSubtree(p);
          q:= p;
          p.nextSibling;
        end;
      end;
    end;
  end;
end;

{ TdomXPathAxisNameFollowingSibling }

function TdomXPathAxisNameFollowingSibling.getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult;
var
  p,q: TdomNode;
begin
  result:= TdomXPathNodeSetResult.create;
  result.axisType:= axisType;
  if assigned(contextNode) then begin
    case contextNode.nodeType of
      ntElement_Node,ntText_Node,ntCDATA_Section_Node,ntEntity_Reference_Node,
      ntProcessing_Instruction_Node,ntComment_Node: begin
        q:= contextNode;
        p:= contextNode.nextSibling;
        while assigned(p) do begin
          if not ( ( (q.nodeType = ntText_Node) or
                     (q.nodeType = ntCDATA_Section_Node) or
                     (q.nodeType = ntEntity_Reference_Node) ) and
                   ( (p.nodeType = ntText_Node) or
                     (p.nodeType = ntCDATA_Section_Node) or
                     (p.nodeType = ntEntity_Reference_Node) ) )
          then result.add(p);
          q:= p;
          p.nextSibling;
        end;
      end;
    end;
  end;
end;

{ TdomXPathAxisNameNamespace }

constructor TdomXPathAxisNameNamespace.create(const AOwner: TdomXPathSyntaxTree;
                                              const value: wideString);
begin
  inherited;
  FPrincipalNodeType:= ntXPath_Namespace_Node;
end;

function TdomXPathAxisNameNamespace.getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult;
var
  PrefixUriList: TUtilsNameValueList;
  cNode: TdomNode;
  I: integer;
begin
  Result:= TdomXPathNodeSetResult.create;
  Result.axisType:= axisType;

  if contextNode.nodeType = ntElement_Node then begin
    PrefixUriList:= TUtilsNameValueList.Create;
    try
      with PrefixUriList do begin
        Sorted := True;
        Duplicates := dupIgnore;
        Add('xml', 'http://www.w3.org/XML/1998/namespace');
        Add('xmlns', 'http://www.w3.org/2000/xmlns/'); 
      end;

      cNode := contextNode;
      while assigned(cNode) do begin
        if cNode.nodeType <> ntElement_Node then break;
        with cNode.Attributes do
          for I:= 0 to Pred(Length) do
            with TdomAttr(Item(I)) do
              case IsXmlnsDecl of
                NSDT_DEFAULT: PrefixUriList.Add('', NodeValue);
                NSDT_PREFIXED: PrefixUriList.Add(LocalName, NodeValue);
              end;
        cNode:= cNode.parentNode;
      end;

      with PrefixUriList do
        for I:= 0 to Pred(Length) do
          Result.addXPathNamespace(contextNode as TdomElement, Values[I], Names[I]);

    finally
      PrefixUriList.Free;
    end;
  end;
end;

{ TdomXPathAxisNameParent }

function TdomXPathAxisNameParent.getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult;
var
  n: TdomNode;
begin
  result:= TdomXPathNodeSetResult.create;
  result.axisType:= axisType;
  if assigned(contextNode) then begin
    case contextNode.nodeType of
      ntElement_Node,ntText_Node,ntCDATA_Section_Node,ntEntity_Reference_Node,
      ntProcessing_Instruction_Node,ntComment_Node:
        n:= contextNode.parentNode;
      ntAttribute_Node:
        n:= TdomAttr(contextNode).ownerElement;
      ntXPath_Namespace_Node:
        n:= TdomXPathNamespace(contextNode).ownerElement;
    else
      n:= nil;
    end;
    if assigned(n)
      then result.add(n);
  end;
end;

{ TdomXPathAxisNamePreceding }

constructor TdomXPathAxisNamePreceding.create(const AOwner: TdomXPathSyntaxTree;
                                              const value: wideString);
begin
  inherited;
  FAxisType:= XPATH_REVERSE_AXIS;
end;

function TdomXPathAxisNamePreceding.getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult;
var
  n: TdomNode;

  procedure addPreceding(const snapshot: TdomXPathNodeSetResult;
                         const node: TdomNode);
  var
    p,q: TdomNode;
  begin
    case node.nodeType of
      ntElement_Node,ntText_Node,ntCDATA_Section_Node,ntEntity_Reference_Node,
      ntProcessing_Instruction_Node,ntComment_Node: begin
        p:= node.previousSibling;
        while assigned(p) do begin
          q:= p.previousSibling;
          if assigned(q) then begin
            if not ( ( (p.nodeType = ntText_Node) or
                       (p.nodeType = ntCDATA_Section_Node) or
                       (p.nodeType = ntEntity_Reference_Node) ) and
                     ( (q.nodeType = ntText_Node) or
                       (q.nodeType = ntCDATA_Section_Node) or
                       (q.nodeType = ntEntity_Reference_Node) ) )
            then snapshot.addSubtree(p);
            p:= q;
          end else begin
            snapshot.addSubtree(p);
            break;
          end;
        end;
      end;
    end;
  end;

begin
  result:= TdomXPathNodeSetResult.create;
  result.axisType:= axisType;
  if assigned(contextNode) then begin
    addPreceding(result,contextNode);
    case contextNode.nodeType of
      ntElement_Node,ntText_Node,ntCDATA_Section_Node,ntEntity_Reference_Node,
      ntProcessing_Instruction_Node,ntComment_Node:
        n:= contextNode.parentNode;
      ntAttribute_Node:
        n:= TdomAttr(contextNode).ownerElement;
      ntXPath_Namespace_Node:
        n:= TdomXPathNamespace(contextNode).ownerElement;
    else
      n:= nil;
    end;
    while assigned(n) do begin
      addPreceding(result,n);
      n:= n.parentNode;
    end;
  end;
end;

{ TdomXPathAxisNamePrecedingSibling }

constructor TdomXPathAxisNamePrecedingSibling.create(const AOwner: TdomXPathSyntaxTree;
                                                     const value: wideString);
begin
  inherited;
  FAxisType:= XPATH_REVERSE_AXIS;
end;

function TdomXPathAxisNamePrecedingSibling.getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult;
var
  p,q: TdomNode;
begin
  result:= TdomXPathNodeSetResult.create;
  result.axisType:= axisType;
  if assigned(contextNode) then begin
    case contextNode.nodeType of
      ntElement_Node,ntText_Node,ntCDATA_Section_Node,ntEntity_Reference_Node,
      ntProcessing_Instruction_Node,ntComment_Node: begin
        p:= contextNode.previousSibling;
        while assigned(p) do begin
          q:= p.previousSibling;
          if assigned(q) then begin
            if not ( ( (p.nodeType = ntText_Node) or
                       (p.nodeType = ntCDATA_Section_Node) or
                       (p.nodeType = ntEntity_Reference_Node) ) and
                     ( (q.nodeType = ntText_Node) or
                       (q.nodeType = ntCDATA_Section_Node) or
                       (q.nodeType = ntEntity_Reference_Node) ) )
            then result.add(p);
            p:= q;
          end else begin
            result.add(p);
            break;
          end;
        end;
      end;
    end;
  end;
end;

{ TdomXPathAxisNameSelf }

function TdomXPathAxisNameSelf.getAxisNodeSnapshot(const contextNode: TdomNode): TdomXPathNodeSetResult;
begin
  result:= TdomXPathNodeSetResult.create;
  result.axisType:= axisType;
  if assigned(contextNode)
    then result.add(contextNode);
end;

{ TdomXPathNodeTest }

function TdomXPathNodeTest.evaluate(const oldSnapshotResult: TdomXPathNodeSetResult;
                                     const principalNodeType: TdomNodeType): TdomXPathNodeSetResult;
begin
  if not assigned(oldSnapshotResult)
    then raise EXPath_Type_Err.create('XPath type error.');

  if left is TdomXPathNameTest then begin
    result:= TdomXPathNameTest(left).evaluate(oldSnapshotResult,principalNodeType);
  end else if left is TdomXPathNodeTypeComment then begin
    result:= TdomXPathNodeTypeComment(left).evaluate(oldSnapshotResult);
  end else if left is TdomXPathNodeTypeText then begin
    result:= TdomXPathNodeTypeText(left).evaluate(oldSnapshotResult);
  end else if left is TdomXPathNodeTypePI then begin
    result:= TdomXPathNodeTypePI(left).evaluate(oldSnapshotResult);
  end else if left is TdomXPathNodeTypeNode then begin
    result:= oldSnapshotResult;
  end else begin
    oldSnapshotResult.free;
    raise EXPath_Type_Err.create('XPath type error.');
  end;
end;

{ TdomXPathPredicate }

function TdomXPathPredicate.evaluate(const oldSnapshotResult: TdomXPathNodeSetResult): TdomXPathNodeSetResult;
var
  contextNode: TdomNode;
  nextPredicateResult: TdomXPathNodeSetResult;
  predicateResult: TdomXPathCustomResult;
  predicateResultAsBoolean: TdomXPathBooleanResult;
  contextPosition: integer;
begin
  if not assigned(oldSnapshotResult)
    then raise EXPath_Type_Err.create('XPath type error.');
  if not (left is TdomXPathExpr) then begin
    oldSnapshotResult.free;
    raise EXPath_Type_Err.create('XPath type error.');
  end;

  try
    result:= TdomXPathNodeSetResult.create;
    result.axisType:= oldSnapshotResult.axisType;
    with oldSnapshotResult do begin
      for contextPosition:= 1 to length do begin
        contextNode:= item(pred(contextPosition));
        try
          predicateResult:= TdomXPathExpr(left).evaluate(contextNode, contextPosition, length);
          if predicateResult is TdomXPathNumberResult then begin
            if predicateResult.asNumber = contextPosition then
              result.add(contextNode);
            predicateResult.free;
          end else begin
            predicateResultAsBoolean:= XPathBooleanFunc(predicateResult);
            if predicateResultAsBoolean.asBoolean then
              result.add(contextNode);
            predicateResultAsBoolean.free;
          end;
        except
          result.free;
          raise;
        end;
      end;
    end;

    if assigned(right) then begin
      if right is TdomXPathPredicate then begin
        nextPredicateResult:= TdomXPathPredicate(right).evaluate(result);
        result:= nextPredicateResult;
      end else begin
        result.free;
        raise EXPath_Type_Err.create('XPath type error.');
      end;
    end;

  finally
    oldSnapshotResult.free;
  end;
end;

{ TdomXPathExpr }

function TdomXPathExpr.evaluate(const contextNode: TdomNode;
                                 const contextPosition,
                                       contextSize: Integer): TdomXPathCustomResult;
begin
  if (left is TdomXPathOrExpr) then begin
    result:= TdomXPathOrExpr(left).evaluate(contextNode, contextPosition, contextSize);
  end else raise EXPath_Type_Err.create('XPath type error.');
end;

{ TdomXPathPrimaryExpr }

function TdomXPathPrimaryExpr.evaluate(const contextNode: TdomNode;
                                        const contextPosition,
                                              contextSize: Integer): TdomXPathCustomResult;
begin
  if (left is TdomXPathVariableReference) then begin
    result:= TdomXPathVariableReference(left).evaluate;
  end else if (left is TdomXPathExpr) then begin
    result:= TdomXPathExpr(left).evaluate(contextNode, contextPosition, contextSize);
  end else if (left is TdomXPathLiteral) then begin
    result:= TdomXPathLiteral(left).evaluate;
  end else if (left is TdomXPathNumber) then begin
    result:= TdomXPathNumber(left).evaluate;
  end else if (left is TdomXPathFunctionCall) then begin
    result:= TdomXPathFunctionCall(left).evaluate(contextNode, contextPosition, contextSize);
  end else raise EXPath_Type_Err.create('XPath type error.');
end;

{ TdomXPathFunctionCall }

constructor TdomXPathFunctionCall.create(const AOwner: TdomXPathSyntaxTree;
                                         const value: wideString);
begin
  inherited;
  FArguments:= TList.Create;
end;

destructor TdomXPathFunctionCall.destroy;
begin
  FArguments.Free;
  inherited;
end;

function TdomXPathFunctionCall.evaluate(const contextNode: TdomNode;
                                        const contextPosition: Integer;
                                        const contextSize: Integer): TdomXPathCustomResult;
var
  NsUri: wideString;
begin
  if assigned(FXPathFunction) then begin
    result := FXPathFunction(contextNode, contextPosition, contextSize, arguments);
  end else begin
    result:= nil;
    // Determine Namespace URI
    NsUri:= lookupNamespaceURI;
    if (NsUri = '') and (FPrefix <> '') then
      raise ENamespace_Err.CreateFmt('Namespace URI of prefix ''%S'' not found.',
                                     [FPrefix]);

    if assigned(ownerSyntaxTree) then with ownerSyntaxTree do
      if assigned(contextNode) then with contextNode do
        if assigned(ownerDocument) then with ownerDocument do
          if assigned(domImplementation) then
            domImplementation.DoRequestXPathFunctionResult(
              NsUri, FLocalName, contextNode, contextPosition, contextSize, arguments, result
            );

    if not assigned(result) then
      raise EXPath_Exception.create('Unknown function name.');
  end;
end;

function TdomXPathFunctionCall.getFunctionName: wideString;
begin
  result := FValue;
end;

function TdomXPathFunctionCall.lookupNamespaceURI: wideString;
begin
  if assigned(ownerSyntaxTree) then begin
    if assigned(ownerSyntaxTree.contextNode)
      then Result := ownerSyntaxTree.contextNode.lookupNamespaceURI(FPrefix)
      else Result := '';
  end else Result := '';
end;

procedure TdomXPathFunctionCall.setFunctionName(const aFunctionName: wideString);
begin
  if aFunctionName <> FValue then begin
    FValue := aFunctionName;
    FPrefix:= xmlExtractPrefix(value);
    FLocalName:= xmlExtractLocalName(value);
    if FPrefix = '' then begin
      if FLocalName = 'last' then begin
        FXPathFunction := XPathFunctionLast;
      end else if FLocalName = 'position' then begin
        FXPathFunction := XPathFunctionPosition;
      end else if FLocalName = 'count' then begin
        FXPathFunction := XPathFunctionCount;
      end else if FLocalName = 'id' then begin
        FXPathFunction := XPathFunctionId;
      end else if FLocalName = 'local-name' then begin
        FXPathFunction := XPathFunctionLocalName;
      end else if FLocalName = 'namespace-uri' then begin
        FXPathFunction := XPathFunctionNamespaceUri;
      end else if FLocalName = 'name' then begin
        FXPathFunction := XPathFunctionName;
      end else if FLocalName = 'string' then begin
        FXPathFunction := XPathFunctionString;
      end else if FLocalName = 'concat' then begin
        FXPathFunction := XPathFunctionConcat;
      end else if FLocalName = 'starts-with' then begin
        FXPathFunction := XPathFunctionStartsWith;
      end else if FLocalName = 'contains' then begin
        FXPathFunction := XPathFunctionContains;
      end else if FLocalName = 'substring-before' then begin
        FXPathFunction := XPathFunctionSubstringBefore;
      end else if FLocalName = 'substring-after' then begin
        FXPathFunction := XPathFunctionSubstringAfter;
      end else if FLocalName = 'substring' then begin
        FXPathFunction := XPathFunctionSubstring;
      end else if FLocalName = 'string-length' then begin
        FXPathFunction := XPathFunctionStringLength;
      end else if FLocalName = 'normalize-space' then begin
        FXPathFunction := XPathFunctionNormalizeSpace;
      end else if FLocalName = 'translate' then begin
        FXPathFunction := XPathFunctionTranslate;
      end else if FLocalName = 'boolean' then begin
        FXPathFunction := XPathFunctionBoolean;
      end else if FLocalName = 'not' then begin
        FXPathFunction := XPathFunctionNot;
      end else if FLocalName = 'true' then begin
        FXPathFunction := XPathFunctionTrue;
      end else if FLocalName = 'false' then begin
        FXPathFunction := XPathFunctionFalse;
      end else if FLocalName = 'lang' then begin
        FXPathFunction := XPathFunctionLang;
      end else if FLocalName = 'number' then begin
        FXPathFunction := XPathFunctionNumber;
      end else if FLocalName = 'sum' then begin
        FXPathFunction := XPathFunctionSum;
      end else if FLocalName = 'floor' then begin
        FXPathFunction := XPathFunctionFloor;
      end else if FLocalName = 'ceiling' then begin
        FXPathFunction := XPathFunctionCeiling;
      end else if FLocalName = 'round' then begin
        FXPathFunction := XPathFunctionRound;
      end else FXPathFunction := nil;
    end else FXPathFunction := nil;
  end;
end;

{ TdomXPathUnionExpr }

function TdomXPathUnionExpr.evaluate(const contextNode: TdomNode;
                                     const contextPosition,
                                           contextSize: Integer): TdomXPathCustomResult;
var
  leftSnapshotResult: TdomXPathNodeSetResult;
begin
  if (left is TdomXPathUnionExpr) and (right is TdomXPathPathExpr) then begin

    leftSnapshotResult:= TdomXPathNodeSetResult(TdomXPathUnionExpr(left).evaluate(contextNode, contextPosition, contextSize));
    leftSnapshotResult.axisType:= XPATH_FORWARD_AXIS;
    try
      result:= TdomXPathPathExpr(right).evaluate(contextNode, contextPosition, contextSize);
      TdomXPathNodeSetResult(result).merge(leftSnapshotResult);
    finally
      leftSnapshotResult.free;
    end;

  end else if (left is TdomXPathPathExpr) and not assigned(right) then begin

    result:= TdomXPathPathExpr(left).evaluate(contextNode, contextPosition, contextSize);
    if result is TdomXPathNodeSetResult
      then result.axisType:= XPATH_FORWARD_AXIS;

  end else raise EXPath_Type_Err.create('XPath type error.');
end;

{ TdomXPathPathExpr }

function TdomXPathPathExpr.addStep(const step: TdomXPathStep): boolean;
begin
  if not assigned(right) then begin
    right:= step;
    result:= true;
  end else begin
    if right is TdomXPathStep
      then result:= TdomXPathStep(right).addStep(step)
      else result:= false;
  end;
end;

function TdomXPathPathExpr.evaluate(const contextNode: TdomNode;
                                     const contextPosition,
                                           contextSize: Integer): TdomXPathCustomResult;
var
  newResult: TdomXPathCustomResult;
begin
  if left is TdomXPathFilterExpr then begin
    // Filter expression plus optional relative location path:
    newResult:= TdomXPathFilterExpr(left).evaluate(contextNode, contextPosition, contextSize);
    if right is TdomXPathStep then begin
      if not (newResult is TdomXPathNodeSetResult) then begin
        newResult.free;
        raise EXPath_Type_Err.create('XPath type error.');
      end;
      result:= TdomXPathStep(right).evaluate(TdomXPathNodeSetResult(newResult));
    end else result:= newResult;
  end else if left is TdomXPathAbsoluteLocationPath then begin
    // Absolute location path:
    if not assigned(contextNode)
      then raise EXPath_Type_Err.create('XPath type error.');
    if not assigned(contextNode.referenceDocument)
      then raise EXPath_Type_Err.create('XPath type error.');
    newResult:= TdomXPathNodeSetResult.create;
    TdomXPathNodeSetResult(newResult).add(contextNode.referenceDocument);
    if right is TdomXPathStep
      then result:= TdomXPathStep(right).evaluate(TdomXPathNodeSetResult(newResult))
      else result:= newResult;
  end else begin
    // Relative location path:
    if not (right is TdomXPathStep)
      then raise EXPath_Type_Err.create('XPath type error.');
    if not assigned(contextNode)
      then raise EXPath_Type_Err.create('XPath type error.');
    newResult:= TdomXPathNodeSetResult.create;
    TdomXPathNodeSetResult(newResult).add(contextNode);
    result:= TdomXPathStep(right).evaluate(TdomXPathNodeSetResult(newResult))
  end;
end;

{ TdomXPathFilterExpr }

function TdomXPathFilterExpr.evaluate(const contextNode: TdomNode;
                                      const contextPosition,
                                            contextSize: Integer): TdomXPathCustomResult;
var
  newResult: TdomXPathCustomResult;
begin
  if (left is TdomXPathFilterExpr) and (right is TdomXPathPredicate)
  then begin
    // Filter expression plus predicate:
    newResult:= TdomXPathFilterExpr(left).evaluate(contextNode, contextPosition, contextSize);
    if not (newResult is TdomXPathNodeSetResult) then begin
      newResult.free;
      raise EXPath_Type_Err.create('XPath type error.');
    end;

    // A predicate filters the node-set with respect to the child axis,
    // so the axis always has to be a forward axis, no matter what axis
    // the previous expression required:
    if newResult is TdomXPathNodeSetResult
      then newResult.axisType:= XPATH_FORWARD_AXIS;

    result:= TdomXPathPredicate(right).evaluate(TdomXPathNodeSetResult(newResult));
  end else if (left is TdomXPathPrimaryExpr) and not assigned(right) then begin
    // PrimaryExpr:
    result:= TdomXPathPrimaryExpr(left).evaluate(contextNode, contextPosition, contextSize);
  end else raise EXPath_Type_Err.create('XPath type error.');
end;

{ TdomXPathOrExpr }

function TdomXPathOrExpr.evaluate(const contextNode: TdomNode;
                                   const contextPosition,
                                         contextSize: Integer): TdomXPathCustomResult;
var
  booleanResult: TdomXPathBooleanResult;
begin
  if (left is TdomXPathOrExpr) and (right is TdomXPathAndExpr) then begin
    booleanResult:= XPathBooleanFunc(TdomXPathOrExpr(left).evaluate(contextNode, contextPosition, contextSize));
    if booleanResult.asBoolean then begin
      result:= booleanResult;
    end else begin
      booleanResult.free;
      result:= XPathBooleanFunc(TdomXPathAndExpr(right).evaluate(contextNode, contextPosition, contextSize));
    end;
  end else if (left is TdomXPathAndExpr) then begin
    result:= TdomXPathAndExpr(left).evaluate(contextNode, contextPosition, contextSize);
  end else raise EXPath_Type_Err.create('XPath type error.');
end;

{ TdomXPathAndExpr }

function TdomXPathAndExpr.evaluate(const contextNode: TdomNode;
                                   const contextPosition,
                                         contextSize: Integer): TdomXPathCustomResult;
var
  booleanResult: TdomXPathBooleanResult;
begin
  if (left is TdomXPathAndExpr) and (right is TdomXPathEqualityExpr) then begin
    booleanResult:= XPathBooleanFunc(TdomXPathAndExpr(left).evaluate(contextNode, contextPosition, contextSize));
    if not booleanResult.asBoolean then begin
      result:= booleanResult;
    end else begin
      booleanResult.free;
      result:= XPathBooleanFunc(TdomXPathEqualityExpr(right).evaluate(contextNode, contextPosition, contextSize));
    end;
  end else if left is TdomXPathEqualityExpr then begin
    result:= TdomXPathEqualityExpr(left).evaluate(contextNode, contextPosition, contextSize);
  end else raise EXPath_Type_Err.create('XPath type error.');
end;

{ TdomXPathIsEqualExpr }

function TdomXPathIsEqualExpr.evaluate(const contextNode: TdomNode;
                                        const contextPosition,
                                              contextSize: Integer): TdomXPathCustomResult;
var
  leftResult,rightResult,swapResult: TdomXPathCustomResult;
  stringResult: TdomXPathStringResult;
  leftBoolean,rightBoolean: TdomXPathBooleanResult;
  leftNumber,rightNumber: TdomXPathNumberResult;
  leftString,rightString: TdomXPathStringResult;
  leftResultString: wideString;
  i,j: integer;
{$IFDEF VER140+}
  ExceptionMaskBackup: TFPUExceptionMask;
{$ENDIF}
begin
  result:= nil;
  if (left is TdomXPathEqualityExpr) and (right is TdomXPathRelationalExpr) then begin
    rightResult:= nil; // Saves one try ... finally block.
    leftResult:= TdomXPathEqualityExpr(left).evaluate(contextNode, contextPosition, contextSize);
    try
      rightResult:= TdomXPathRelationalExpr(right).evaluate(contextNode, contextPosition, contextSize);

      // Make sure, that if at least one set takes part in the comparision,
      // it is assigned to rightResult:
      if rightResult is TdomXPathNodeSetResult then begin
        swapResult:= leftResult;
        leftResult:= rightResult;
        rightResult:= swapResult;
      end;

      if leftResult is TdomXPathNodeSetResult then begin
        if rightResult is TdomXPathNodeSetResult then begin
          for i:= 0 to pred(TdomXPathNodeSetResult(leftResult).length) do begin
            leftResultString:= TdomXPathNodeSetResult(leftResult).item(i).XPathStringValue;
            for j:= 0 to pred(TdomXPathNodeSetResult(rightResult).length) do begin
              if TdomXPathNodeSetResult(rightResult).item(j).XPathStringValue = leftResultString then begin
                result:= TdomXPathBooleanResult.create(true);
                exit;
              end;
            end;
          end;
          result:= TdomXPathBooleanResult.create(false);
          exit;
        end else if (rightResult is TdomXPathNumberResult) or
                    (rightResult is TdomXPathBooleanResult) or
                    (rightResult is TdomXPathStringResult)
        then begin
          stringResult:= XPathStringFunc(rightResult);
          for i:= 0 to pred(TdomXPathNodeSetResult(leftResult).length) do begin
            if TdomXPathNodeSetResult(leftResult).item(i).XPathStringValue = stringResult.asWideString then begin
              result:= TdomXPathBooleanResult.create(true);
              rightResult:= stringResult;  // Re-assignment is required for correct freeing the TdomXPathCustomResult below.
              exit;
            end;
          end;
          result:= TdomXPathBooleanResult.create(false);
          rightResult:= stringResult;  // Re-assignment is required for correct freeing the TdomXPathCustomResult below.
          exit;
        end;
      end else if (leftResult is TdomXPathBooleanResult) or
                  (rightResult is TdomXPathBooleanResult)
      then begin
        leftBoolean:= XPathBooleanFunc(leftResult);
        rightBoolean:= XPathBooleanFunc(rightResult);
        if leftBoolean.asBoolean = rightBoolean.asBoolean
          then result:= TdomXPathBooleanResult.create(true)
          else result:= TdomXPathBooleanResult.create(false);
        leftResult:= leftBoolean;    // Re-assignment is required for correct
        rightResult:= rightBoolean;  // freeing the TdomXPathCustomResult below.
      end else if (leftResult is TdomXPathNumberResult) or
                  (rightResult is TdomXPathNumberResult)
      then begin
        leftNumber:= XPathNumberFunc(leftResult);
        rightNumber:= XPathNumberFunc(rightResult);
{$IFDEF VER140+}
        ExceptionMaskBackup:= SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$ENDIF}
        try
          if leftNumber.asNumber = rightNumber.asNumber
            then result:= TdomXPathBooleanResult.create(true)
            else result:= TdomXPathBooleanResult.create(false);
{$IFDEF VER140+}
        finally
          SetExceptionMask(ExceptionMaskBackup);
{$ELSE}
        except
          if (isNaN(leftNumber.asNumber) and  isNaN(rightNumber.asNumber)) or
             (sign(leftNumber.asNumber) = sign(rightNumber.asNumber))
            then result:= TdomXPathBooleanResult.create(true)
            else result:= TdomXPathBooleanResult.create(false);
{$ENDIF}
        end;
        leftResult:= leftNumber;    // Re-assignment is required for correct
        rightResult:= rightNumber;  // freeing the TdomXPathCustomResult below.
      end else begin
        leftString:= XPathStringFunc(leftResult);
        rightString:= XPathStringFunc(rightResult);
        if leftString.asWideString = rightString.asWideString
          then result:= TdomXPathBooleanResult.create(true)
          else result:= TdomXPathBooleanResult.create(false);
        leftResult:= leftString;    // Re-assignment is required for correct
        rightResult:= rightString;  // freeing the TdomXPathCustomResult below.
      end;

    finally
      rightResult.free;
      leftResult.free;
    end;
  end else if left is TdomXPathRelationalExpr then begin
    result:= TdomXPathRelationalExpr(left).evaluate(contextNode, contextPosition, contextSize);
  end else raise EXPath_Type_Err.create('XPath type error.');
end;

{ TdomXPathIsNotEqualExpr }

function TdomXPathIsNotEqualExpr.evaluate(const contextNode: TdomNode;
                                           const contextPosition,
                                                 contextSize: Integer): TdomXPathCustomResult;
var
  leftResult,rightResult,swapResult: TdomXPathCustomResult;
  stringResult: TdomXPathStringResult;
  leftBoolean,rightBoolean: TdomXPathBooleanResult;
  leftNumber,rightNumber: TdomXPathNumberResult;
  leftString,rightString: TdomXPathStringResult;
  leftResultString: wideString;
  i,j: integer;
{$IFDEF VER140+}
  ExceptionMaskBackup: TFPUExceptionMask;
{$ENDIF}
begin
  result:= nil;
  if (left is TdomXPathEqualityExpr) and (right is TdomXPathRelationalExpr) then begin
    rightResult:= nil; // Saves one try ... finally block.
    leftResult:= TdomXPathEqualityExpr(left).evaluate(contextNode, contextPosition, contextSize);
    try
      rightResult:= TdomXPathRelationalExpr(right).evaluate(contextNode, contextPosition, contextSize);

      // Make sure, that if at least one set takes part in the comparision,
      // it is assigned to rightResult:
      if rightResult is TdomXPathNodeSetResult then begin
        swapResult:= leftResult;
        leftResult:= rightResult;
        rightResult:= swapResult;
      end;

      if leftResult is TdomXPathNodeSetResult then begin
        if rightResult is TdomXPathNodeSetResult then begin
          for i:= 0 to pred(TdomXPathNodeSetResult(leftResult).length) do begin
            leftResultString:= TdomXPathNodeSetResult(leftResult).item(i).XPathStringValue;
            for j:= 0 to pred(TdomXPathNodeSetResult(rightResult).length) do begin
              if TdomXPathNodeSetResult(rightResult).item(j).XPathStringValue <> leftResultString then begin
                result:= TdomXPathBooleanResult.create(true);
                exit;
              end;
            end;
          end;
          result:= TdomXPathBooleanResult.create(false);
          exit;
        end else if (rightResult is TdomXPathNumberResult) or
                    (rightResult is TdomXPathBooleanResult) or
                    (rightResult is TdomXPathStringResult)
        then begin
          stringResult:= XPathStringFunc(rightResult);
          for i:= 0 to pred(TdomXPathNodeSetResult(leftResult).length) do begin
            if TdomXPathNodeSetResult(leftResult).item(i).XPathStringValue <> stringResult.asWideString then begin
              result:= TdomXPathBooleanResult.create(true);
              rightResult:= stringResult;  // Re-assignment is required for correct freeing the TdomXPathCustomResult below.
              exit;
            end;
          end;
          result:= TdomXPathBooleanResult.create(false);
          rightResult:= stringResult;  // Re-assignment is required for correct freeing the TdomXPathCustomResult below.
          exit;
        end;
      end else if (leftResult is TdomXPathBooleanResult) or
                  (rightResult is TdomXPathBooleanResult)
      then begin
        leftBoolean:= XPathBooleanFunc(leftResult);
        rightBoolean:= XPathBooleanFunc(rightResult);
        if leftBoolean.asBoolean <> rightBoolean.asBoolean
          then result:= TdomXPathBooleanResult.create(true)
          else result:= TdomXPathBooleanResult.create(false);
        leftResult:= leftBoolean;    // Re-assignment is required for correct
        rightResult:= rightBoolean;  // freeing the TdomXPathCustomResult below.
      end else if (leftResult is TdomXPathNumberResult) or
                  (rightResult is TdomXPathNumberResult)
      then begin
        leftNumber:= XPathNumberFunc(leftResult);
        rightNumber:= XPathNumberFunc(rightResult);
{$IFDEF VER140+}
        ExceptionMaskBackup:= SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$ENDIF}
        try
          if leftNumber.asNumber <> rightNumber.asNumber
            then result:= TdomXPathBooleanResult.create(true)
            else result:= TdomXPathBooleanResult.create(false);
{$IFDEF VER140+}
        finally
          SetExceptionMask(ExceptionMaskBackup);
{$ELSE}
        except
          if (isNaN(leftNumber.asNumber) and not isNaN(rightNumber.asNumber)) or
             (isNaN(rightNumber.asNumber) and not isNaN(leftNumber.asNumber)) or
             (sign(leftNumber.asNumber) <> sign(rightNumber.asNumber))
            then result:= TdomXPathBooleanResult.create(true)
            else result:= TdomXPathBooleanResult.create(false);
{$ENDIF}
        end;
        leftResult:= leftNumber;    // Re-assignment is required for correct
        rightResult:= rightNumber;  // freeing the TdomXPathCustomResult below.
      end else begin
        leftString:= XPathStringFunc(leftResult);
        rightString:= XPathStringFunc(rightResult);
        if leftString.asWideString <> rightString.asWideString
          then result:= TdomXPathBooleanResult.create(true)
          else result:= TdomXPathBooleanResult.create(false);
        leftResult:= leftString;    // Re-assignment is required for correct
        rightResult:= rightString;  // freeing the TdomXPathCustomResult below.
      end;

    finally
      rightResult.free;
      leftResult.free;
    end;
  end else if left is TdomXPathRelationalExpr then begin
    result:= TdomXPathRelationalExpr(left).evaluate(contextNode, contextPosition, contextSize);
  end else raise EXPath_Type_Err.create('XPath type error.');
end;

{ TdomXPathLessThanExpr }

function TdomXPathLessThanExpr.evaluate(const contextNode: TdomNode;
                                         const contextPosition,
                                               contextSize: Integer): TdomXPathCustomResult;
var
  leftResult,rightResult: TdomXPathCustomResult;
  stringResult: TdomXPathStringResult;
  leftNumber,rightNumber: TdomXPathNumberResult;
  leftResultString: wideString;
  i,j: integer;
{$IFDEF VER140+}
  ExceptionMaskBackup: TFPUExceptionMask;
{$ENDIF}
begin
  if (left is TdomXPathRelationalExpr) and (right is TdomXPathAdditiveExpr) then begin
    rightResult:= nil; // Saves one try ... finally block.
    leftResult:= TdomXPathRelationalExpr(left).evaluate(contextNode, contextPosition, contextSize);
    try
      rightResult:= TdomXPathAdditiveExpr(right).evaluate(contextNode, contextPosition, contextSize);

      if (leftResult is TdomXPathNodeSetResult) and
         (rightResult is TdomXPathNodeSetResult)
      then begin
        for i:= 0 to pred(TdomXPathNodeSetResult(leftResult).length) do begin
          leftResultString:= TdomXPathNodeSetResult(leftResult).item(i).XPathStringValue;
          for j:= 0 to pred(TdomXPathNodeSetResult(rightResult).length) do begin
            if TdomXPathNodeSetResult(rightResult).item(j).XPathStringValue < leftResultString then begin
              result:= TdomXPathBooleanResult.create(true);
              exit;
            end;
          end;
        end;
        result:= TdomXPathBooleanResult.create(false);
        exit;
      end else if (leftResult is TdomXPathNodeSetResult) and
                  ( (rightResult is TdomXPathNumberResult) or
                    (rightResult is TdomXPathBooleanResult) or
                    (rightResult is TdomXPathStringResult) )
      then begin
        stringResult:= XPathStringFunc(rightResult);
        for i:= 0 to pred(TdomXPathNodeSetResult(leftResult).length) do begin
          if TdomXPathNodeSetResult(leftResult).item(i).XPathStringValue < stringResult.asWideString then begin
            result:= TdomXPathBooleanResult.create(true);
            rightResult:= stringResult;  // Re-assignment is required for correct freeing the TdomXPathCustomResult below.
            exit;
          end;
        end;
        result:= TdomXPathBooleanResult.create(false);
        rightResult:= stringResult;  // Re-assignment is required for correct freeing the TdomXPathCustomResult below.
        exit;
      end else if ( (leftResult is TdomXPathNumberResult) or
                    (leftResult is TdomXPathBooleanResult) or
                    (leftResult is TdomXPathStringResult) ) and
                  (rightResult is TdomXPathNodeSetResult)
      then begin
        stringResult:= XPathStringFunc(leftResult);
        for i:= 0 to pred(TdomXPathNodeSetResult(rightResult).length) do begin
          if stringResult.asWideString < TdomXPathNodeSetResult(rightResult).item(i).XPathStringValue then begin
            result:= TdomXPathBooleanResult.create(true);
            leftResult:= stringResult;  // Re-assignment is required for correct freeing the TdomXPathCustomResult below.
            exit;
          end;
        end;
        result:= TdomXPathBooleanResult.create(false);
        leftResult:= stringResult;  // Re-assignment is required for correct freeing the TdomXPathCustomResult below.
        exit;
      end else begin
        leftNumber:= XPathNumberFunc(leftResult);
        rightNumber:= XPathNumberFunc(rightResult);
{$IFDEF VER140+}
        ExceptionMaskBackup:= SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$ENDIF}
        try
          if leftNumber.asNumber < rightNumber.asNumber
            then result:= TdomXPathBooleanResult.create(true)
            else result:= TdomXPathBooleanResult.create(false);
{$IFDEF VER140+}
        finally
          SetExceptionMask(ExceptionMaskBackup);
{$ELSE}
        except
          result:= TdomXPathBooleanResult.create(false);
{$ENDIF}
        end;
        leftResult:= leftNumber;    // Re-assignment is required for correct
        rightResult:= rightNumber;  // freeing the TdomXPathCustomResult below.
      end;

    finally
      rightResult.free;
      leftResult.free;
    end;
  end else if left is TdomXPathAdditiveExpr then begin
    result:= TdomXPathAdditiveExpr(left).evaluate(contextNode, contextPosition, contextSize);
  end else raise EXPath_Type_Err.create('XPath type error.');
end;

{ TdomXPathLessThanOrEqualExpr }

function TdomXPathLessThanOrEqualExpr.evaluate(const contextNode: TdomNode;
                                                const contextPosition,
                                                      contextSize: Integer): TdomXPathCustomResult;
var
  leftResult,rightResult: TdomXPathCustomResult;
  stringResult: TdomXPathStringResult;
  leftNumber,rightNumber: TdomXPathNumberResult;
  leftResultString: wideString;
  i,j: integer;
{$IFDEF VER140+}
  ExceptionMaskBackup: TFPUExceptionMask;
{$ENDIF}
begin
  if (left is TdomXPathRelationalExpr) and (right is TdomXPathAdditiveExpr) then begin
    rightResult:= nil; // Saves one try ... finally block.
    leftResult:= TdomXPathRelationalExpr(left).evaluate(contextNode, contextPosition, contextSize);
    try
      rightResult:= TdomXPathAdditiveExpr(right).evaluate(contextNode, contextPosition, contextSize);

      if (leftResult is TdomXPathNodeSetResult) and
         (rightResult is TdomXPathNodeSetResult)
      then begin
        for i:= 0 to pred(TdomXPathNodeSetResult(leftResult).length) do begin
          leftResultString:= TdomXPathNodeSetResult(leftResult).item(i).XPathStringValue;
          for j:= 0 to pred(TdomXPathNodeSetResult(rightResult).length) do begin
            if TdomXPathNodeSetResult(rightResult).item(j).XPathStringValue <= leftResultString then begin
              result:= TdomXPathBooleanResult.create(true);
              exit;
            end;
          end;
        end;
        result:= TdomXPathBooleanResult.create(false);
        exit;
      end else if (leftResult is TdomXPathNodeSetResult) and
                  ( (rightResult is TdomXPathNumberResult) or
                    (rightResult is TdomXPathBooleanResult) or
                    (rightResult is TdomXPathStringResult) )
      then begin
        stringResult:= XPathStringFunc(rightResult);
        for i:= 0 to pred(TdomXPathNodeSetResult(leftResult).length) do begin
          if TdomXPathNodeSetResult(leftResult).item(i).XPathStringValue <= stringResult.asWideString then begin
            result:= TdomXPathBooleanResult.create(true);
            rightResult:= stringResult;  // Re-assignment is required for correct freeing the TdomXPathCustomResult below.
            exit;
          end;
        end;
        result:= TdomXPathBooleanResult.create(false);
        rightResult:= stringResult;  // Re-assignment is required for correct freeing the TdomXPathCustomResult below.
        exit;
      end else if ( (leftResult is TdomXPathNumberResult) or
                    (leftResult is TdomXPathBooleanResult) or
                    (leftResult is TdomXPathStringResult) ) and
                  (rightResult is TdomXPathNodeSetResult)
      then begin
        stringResult:= XPathStringFunc(leftResult);
        for i:= 0 to pred(TdomXPathNodeSetResult(rightResult).length) do begin
          if stringResult.asWideString <= TdomXPathNodeSetResult(rightResult).item(i).XPathStringValue then begin
            result:= TdomXPathBooleanResult.create(true);
            leftResult:= stringResult;  // Re-assignment is required for correct freeing the TdomXPathCustomResult below.
            exit;
          end;
        end;
        result:= TdomXPathBooleanResult.create(false);
        leftResult:= stringResult;  // Re-assignment is required for correct freeing the TdomXPathCustomResult below.
        exit;
      end else begin
        leftNumber:= XPathNumberFunc(leftResult);
        rightNumber:= XPathNumberFunc(rightResult);
{$IFDEF VER140+}
        ExceptionMaskBackup:= SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$ENDIF}
        try
          if leftNumber.asNumber <= rightNumber.asNumber
            then result:= TdomXPathBooleanResult.create(true)
            else result:= TdomXPathBooleanResult.create(false);
{$IFDEF VER140+}
        finally
          SetExceptionMask(ExceptionMaskBackup);
{$ELSE}
        except
          if (isNaN(leftNumber.asNumber) and not isNaN(rightNumber.asNumber)) or
             (isNaN(rightNumber.asNumber) and not isNaN(leftNumber.asNumber)) or
             (sign(leftNumber.asNumber) <> sign(rightNumber.asNumber))
            then result:= TdomXPathBooleanResult.create(true)
            else result:= TdomXPathBooleanResult.create(false);
{$ENDIF}
        end;
        leftResult:= leftNumber;    // Re-assignment is required for correct
        rightResult:= rightNumber;  // freeing the TdomXPathCustomResult below.
      end;

    finally
      rightResult.free;
      leftResult.free;
    end;
  end else if left is TdomXPathAdditiveExpr then begin
    result:= TdomXPathAdditiveExpr(left).evaluate(contextNode, contextPosition, contextSize);
  end else raise EXPath_Type_Err.create('XPath type error.');
end;

{ TdomXPathGreaterThanExpr }

function TdomXPathGreaterThanExpr.evaluate(const contextNode: TdomNode;
                                            const contextPosition,
                                                  contextSize: Integer): TdomXPathCustomResult;
var
  leftResult,rightResult: TdomXPathCustomResult;
  stringResult: TdomXPathStringResult;
  leftNumber,rightNumber: TdomXPathNumberResult;
  leftResultString: wideString;
  i,j: integer;
{$IFDEF VER140+}
  ExceptionMaskBackup: TFPUExceptionMask;
{$ENDIF}
begin
  if (left is TdomXPathRelationalExpr) and (right is TdomXPathAdditiveExpr) then begin
    rightResult:= nil; // Saves one try ... finally block.
    leftResult:= TdomXPathRelationalExpr(left).evaluate(contextNode, contextPosition, contextSize);
    try
      rightResult:= TdomXPathAdditiveExpr(right).evaluate(contextNode, contextPosition, contextSize);

      if (leftResult is TdomXPathNodeSetResult) and
         (rightResult is TdomXPathNodeSetResult)
      then begin
        for i:= 0 to pred(TdomXPathNodeSetResult(leftResult).length) do begin
          leftResultString:= TdomXPathNodeSetResult(leftResult).item(i).XPathStringValue;
          for j:= 0 to pred(TdomXPathNodeSetResult(rightResult).length) do begin
            if TdomXPathNodeSetResult(rightResult).item(j).XPathStringValue > leftResultString then begin
              result:= TdomXPathBooleanResult.create(true);
              exit;
            end;
          end;
        end;
        result:= TdomXPathBooleanResult.create(false);
        exit;
      end else if (leftResult is TdomXPathNodeSetResult) and
                  ( (rightResult is TdomXPathNumberResult) or
                    (rightResult is TdomXPathBooleanResult) or
                    (rightResult is TdomXPathStringResult) )
      then begin
        stringResult:= XPathStringFunc(rightResult);
        for i:= 0 to pred(TdomXPathNodeSetResult(leftResult).length) do begin
          if TdomXPathNodeSetResult(leftResult).item(i).XPathStringValue > stringResult.asWideString then begin
            result:= TdomXPathBooleanResult.create(true);
            rightResult:= stringResult;  // Re-assignment is required for correct freeing the TdomXPathCustomResult below.
            exit;
          end;
        end;
        result:= TdomXPathBooleanResult.create(false);
        rightResult:= stringResult;  // Re-assignment is required for correct freeing the TdomXPathCustomResult below.
        exit;
      end else if ( (leftResult is TdomXPathNumberResult) or
                    (leftResult is TdomXPathBooleanResult) or
                    (leftResult is TdomXPathStringResult) ) and
                  (rightResult is TdomXPathNodeSetResult)
      then begin
        stringResult:= XPathStringFunc(leftResult);
        for i:= 0 to pred(TdomXPathNodeSetResult(rightResult).length) do begin
          if stringResult.asWideString > TdomXPathNodeSetResult(rightResult).item(i).XPathStringValue then begin
            result:= TdomXPathBooleanResult.create(true);
            leftResult:= stringResult;  // Re-assignment is required for correct freeing the TdomXPathCustomResult below.
            exit;
          end;
        end;
        result:= TdomXPathBooleanResult.create(false);
        leftResult:= stringResult;  // Re-assignment is required for correct freeing the TdomXPathCustomResult below.
        exit;
      end else begin
        leftNumber:= XPathNumberFunc(leftResult);
        rightNumber:= XPathNumberFunc(rightResult);
{$IFDEF VER140+}
        ExceptionMaskBackup:= SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$ENDIF}
        try
          if leftNumber.asNumber > rightNumber.asNumber
            then result:= TdomXPathBooleanResult.create(true)
            else result:= TdomXPathBooleanResult.create(false);
{$IFDEF VER140+}
        finally
          SetExceptionMask(ExceptionMaskBackup);
{$ELSE}
        except
          result:= TdomXPathBooleanResult.create(false);
{$ENDIF}
        end;
        leftResult:= leftNumber;    // Re-assignment is required for correct
        rightResult:= rightNumber;  // freeing the TdomXPathCustomResult below.
      end;

    finally
      rightResult.free;
      leftResult.free;
    end;
  end else if left is TdomXPathAdditiveExpr then begin
    result:= TdomXPathAdditiveExpr(left).evaluate(contextNode, contextPosition, contextSize);
  end else raise EXPath_Type_Err.create('XPath type error.');
end;

{ TdomXPathGreaterThanOrEqualExpr }

function TdomXPathGreaterThanOrEqualExpr.evaluate(const contextNode: TdomNode;
                                                   const contextPosition,
                                                         contextSize: Integer): TdomXPathCustomResult;
var
  leftResult,rightResult: TdomXPathCustomResult;
  stringResult: TdomXPathStringResult;
  leftNumber,rightNumber: TdomXPathNumberResult;
  leftResultString: wideString;
  i,j: integer;
{$IFDEF VER140+}
  ExceptionMaskBackup: TFPUExceptionMask;
{$ENDIF}
begin
  if (left is TdomXPathRelationalExpr) and (right is TdomXPathAdditiveExpr) then begin
    rightResult:= nil; // Saves one try ... finally block.
    leftResult:= TdomXPathRelationalExpr(left).evaluate(contextNode, contextPosition, contextSize);
    try
      rightResult:= TdomXPathAdditiveExpr(right).evaluate(contextNode, contextPosition, contextSize);

      if (leftResult is TdomXPathNodeSetResult) and
         (rightResult is TdomXPathNodeSetResult)
      then begin
        for i:= 0 to pred(TdomXPathNodeSetResult(leftResult).length) do begin
          leftResultString:= TdomXPathNodeSetResult(leftResult).item(i).XPathStringValue;
          for j:= 0 to pred(TdomXPathNodeSetResult(rightResult).length) do begin
            if TdomXPathNodeSetResult(rightResult).item(j).XPathStringValue >= leftResultString then begin
              result:= TdomXPathBooleanResult.create(true);
              exit;
            end;
          end;
        end;
        result:= TdomXPathBooleanResult.create(false);
        exit;
      end else if (leftResult is TdomXPathNodeSetResult) and
                  ( (rightResult is TdomXPathNumberResult) or
                    (rightResult is TdomXPathBooleanResult) or
                    (rightResult is TdomXPathStringResult) )
      then begin
        stringResult:= XPathStringFunc(rightResult);
        for i:= 0 to pred(TdomXPathNodeSetResult(leftResult).length) do begin
          if TdomXPathNodeSetResult(leftResult).item(i).XPathStringValue >= stringResult.asWideString then begin
            result:= TdomXPathBooleanResult.create(true);
            rightResult:= stringResult;  // Re-assignment is required for correct freeing the TdomXPathCustomResult below.
            exit;
          end;
        end;
        result:= TdomXPathBooleanResult.create(false);
        rightResult:= stringResult;  // Re-assignment is required for correct freeing the TdomXPathCustomResult below.
        exit;
      end else if ( (leftResult is TdomXPathNumberResult) or
                    (leftResult is TdomXPathBooleanResult) or
                    (leftResult is TdomXPathStringResult) ) and
                  (rightResult is TdomXPathNodeSetResult)
      then begin
        stringResult:= XPathStringFunc(leftResult);
        for i:= 0 to pred(TdomXPathNodeSetResult(rightResult).length) do begin
          if stringResult.asWideString >= TdomXPathNodeSetResult(rightResult).item(i).XPathStringValue then begin
            result:= TdomXPathBooleanResult.create(true);
            leftResult:= stringResult;  // Re-assignment is required for correct freeing the TdomXPathCustomResult below.
            exit;
          end;
        end;
        result:= TdomXPathBooleanResult.create(false);
        leftResult:= stringResult;  // Re-assignment is required for correct freeing the TdomXPathCustomResult below.
        exit;
      end else begin
        leftNumber:= XPathNumberFunc(leftResult);
        rightNumber:= XPathNumberFunc(rightResult);
{$IFDEF VER140+}
        ExceptionMaskBackup:= SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$ENDIF}
        try
          if leftNumber.asNumber >= rightNumber.asNumber
            then result:= TdomXPathBooleanResult.create(true)
            else result:= TdomXPathBooleanResult.create(false);
{$IFDEF VER140+}
        finally
          SetExceptionMask(ExceptionMaskBackup);
{$ELSE}
        except
          if (isNaN(leftNumber.asNumber) and not isNaN(rightNumber.asNumber)) or
             (isNaN(rightNumber.asNumber) and not isNaN(leftNumber.asNumber)) or
             (sign(leftNumber.asNumber) <> sign(rightNumber.asNumber))
            then result:= TdomXPathBooleanResult.create(true)
            else result:= TdomXPathBooleanResult.create(false);
{$ENDIF}
        end;
        leftResult:= leftNumber;    // Re-assignment is required for correct
        rightResult:= rightNumber;  // freeing the TdomXPathCustomResult below.
      end;

    finally
      rightResult.free;
      leftResult.free;
    end;
  end else if left is TdomXPathAdditiveExpr then begin
    result:= TdomXPathAdditiveExpr(left).evaluate(contextNode, contextPosition, contextSize);
  end else raise EXPath_Type_Err.create('XPath type error.');
end;

{ TdomXPathPlusExpr }

function TdomXPathPlusExpr.evaluate(const contextNode: TdomNode;
                                     const contextPosition,
                                           contextSize: Integer): TdomXPathCustomResult;
var
  leftNumber,rightNumber: TdomXPathNumberResult;
{$IFDEF VER140+}
  ExceptionMaskBackup: TFPUExceptionMask;
{$ENDIF}
begin
  if (left is TdomXPathAdditiveExpr) and (right is TdomXPathMultiplicativeExpr) then begin
    rightNumber:= nil; // Saves one try ... finally block.
    leftNumber:= XPathNumberFunc(TdomXPathAdditiveExpr(left).evaluate(contextNode, contextPosition, contextSize));
    try
      rightNumber:= XPathNumberFunc(TdomXPathMultiplicativeExpr(right).evaluate(contextNode, contextPosition, contextSize));
{$IFDEF VER140+}
      ExceptionMaskBackup:= SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$ENDIF}
      try
        result:= TdomXPathNumberResult.create(leftNumber.asNumber + rightNumber.asNumber);
{$IFDEF VER140+}
      finally
        SetExceptionMask(ExceptionMaskBackup);
{$ELSE}
      except
        result:= TdomXPathNumberResult.create(NaN);
{$ENDIF}
      end;
    finally
      rightNumber.Free;
      leftNumber.free;
    end;
  end else if left is TdomXPathMultiplicativeExpr then begin
    result:= TdomXPathMultiplicativeExpr(left).evaluate(contextNode, contextPosition, contextSize);
  end else raise EXPath_Type_Err.create('XPath type error.');
end;

{ TdomXPathMinusExpr }

function TdomXPathMinusExpr.evaluate(const contextNode: TdomNode;
                                      const contextPosition,
                                            contextSize: Integer): TdomXPathCustomResult;
var
  leftNumber,rightNumber: TdomXPathNumberResult;
{$IFDEF VER140+}
  ExceptionMaskBackup: TFPUExceptionMask;
{$ENDIF}
begin
  if (left is TdomXPathAdditiveExpr) and (right is TdomXPathMultiplicativeExpr) then begin
    rightNumber:= nil; // Saves one try ... finally block.
    leftNumber:= XPathNumberFunc(TdomXPathAdditiveExpr(left).evaluate(contextNode, contextPosition, contextSize));
    try
      rightNumber:= XPathNumberFunc(TdomXPathMultiplicativeExpr(right).evaluate(contextNode, contextPosition, contextSize));
{$IFDEF VER140+}
      ExceptionMaskBackup:= SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$ENDIF}
      try
        result:= TdomXPathNumberResult.create(leftNumber.asNumber - rightNumber.asNumber);
{$IFDEF VER140+}
      finally
        SetExceptionMask(ExceptionMaskBackup);
{$ELSE}
      except
        result:= TdomXPathNumberResult.create(NaN);
{$ENDIF}
      end;
    finally
      rightNumber.Free;
      leftNumber.free;
    end;
  end else if left is TdomXPathMultiplicativeExpr then begin
    result:= TdomXPathMultiplicativeExpr(left).evaluate(contextNode, contextPosition, contextSize);
  end else raise EXPath_Type_Err.create('XPath type error.');
end;

{ TdomXPathMultiplyExpr }

function TdomXPathMultiplyExpr.evaluate(const contextNode: TdomNode;
                                         const contextPosition,
                                               contextSize: Integer): TdomXPathCustomResult;
var
  leftNumber,rightNumber: TdomXPathNumberResult;
{$IFDEF VER140+}
  ExceptionMaskBackup: TFPUExceptionMask;
{$ENDIF}
begin
  if (left is TdomXPathMultiplicativeExpr) and (right is TdomXPathUnaryExpr) then begin
    rightNumber:= nil; // Saves one try ... finally block.
    leftNumber:= XPathNumberFunc(TdomXPathMultiplicativeExpr(left).evaluate(contextNode, contextPosition, contextSize));
    try
      rightNumber:= XPathNumberFunc(TdomXPathUnaryExpr(right).evaluate(contextNode, contextPosition, contextSize));
{$IFDEF VER140+}
      ExceptionMaskBackup:= SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$ENDIF}
      try
        result:= TdomXPathNumberResult.create(leftNumber.asNumber * rightNumber.asNumber);
{$IFDEF VER140+}
      finally
        SetExceptionMask(ExceptionMaskBackup);
{$ELSE}
      except
        result:= TdomXPathNumberResult.create(NaN);
{$ENDIF}
      end;
    finally
      rightNumber.Free;
      leftNumber.free;
    end;
  end else if left is TdomXPathUnaryExpr then begin
    result:= TdomXPathUnaryExpr(left).evaluate(contextNode, contextPosition, contextSize);
  end else raise EXPath_Type_Err.create('XPath type error.');
end;

{ TdomXPathDivExpr }

function TdomXPathDivExpr.evaluate(const contextNode: TdomNode;
                                    const contextPosition,
                                          contextSize: Integer): TdomXPathCustomResult;
var
  leftNumber,rightNumber: TdomXPathNumberResult;
{$IFDEF VER140+}
  ExceptionMaskBackup: TFPUExceptionMask;
{$ENDIF}
begin
  if (left is TdomXPathMultiplicativeExpr) and (right is TdomXPathUnaryExpr) then begin
    rightNumber:= nil; // Saves one try ... finally block.
    leftNumber:= XPathNumberFunc(TdomXPathMultiplicativeExpr(left).evaluate(contextNode, contextPosition, contextSize));
    try
      rightNumber:= XPathNumberFunc(TdomXPathUnaryExpr(right).evaluate(contextNode, contextPosition, contextSize));
{$IFDEF VER140+}
      ExceptionMaskBackup:= SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$ENDIF}
      try
        result:= TdomXPathNumberResult.create(leftNumber.asNumber / rightNumber.asNumber);
{$IFDEF VER140+}
      finally
        SetExceptionMask(ExceptionMaskBackup);
{$ELSE}
      except
        result:= TdomXPathNumberResult.create(NaN);
{$ENDIF}
      end;
    finally
      rightNumber.Free;
      leftNumber.Free;
    end;
  end else if left is TdomXPathUnaryExpr then begin
    result:= TdomXPathUnaryExpr(left).evaluate(contextNode, contextPosition, contextSize);
  end else raise EXPath_Type_Err.create('XPath type error.');
end;

{ TdomXPathModExpr }

function TdomXPathModExpr.evaluate(const contextNode: TdomNode;
                                    const contextPosition,
                                          contextSize: Integer): TdomXPathCustomResult;
var
  leftNumber,rightNumber: TdomXPathNumberResult;
{$IFDEF VER140+}
  ExceptionMaskBackup: TFPUExceptionMask;
{$ENDIF}
begin
  if (left is TdomXPathMultiplicativeExpr) and (right is TdomXPathUnaryExpr) then begin
    rightNumber:= nil; // Saves one try ... finally block.
    leftNumber:= XPathNumberFunc(TdomXPathMultiplicativeExpr(left).evaluate(contextNode, contextPosition, contextSize));
    try
      rightNumber:= XPathNumberFunc(TdomXPathUnaryExpr(right).evaluate(contextNode, contextPosition, contextSize));
{$IFDEF VER140+}
      ExceptionMaskBackup:= SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$ENDIF}
      try
        result:= TdomXPathNumberResult.create(leftNumber.asNumber - trunc(leftNumber.asNumber / rightNumber.asNumber) * rightNumber.asNumber);
{$IFDEF VER140+}
      finally
        SetExceptionMask(ExceptionMaskBackup);
{$ELSE}
      except
        result:= TdomXPathNumberResult.create(NaN);
{$ENDIF}
      end;
    finally
      rightNumber.Free;
      leftNumber.free;
    end;
  end else if left is TdomXPathUnaryExpr then begin
    result:= TdomXPathUnaryExpr(left).evaluate(contextNode, contextPosition, contextSize);
  end else raise EXPath_Type_Err.create('XPath type error.');
end;

{ TdomXPathUnaryExpr }

function TdomXPathUnaryExpr.evaluate(const contextNode: TdomNode;
                                     const contextPosition,
                                           contextSize: Integer): TdomXPathCustomResult;
var
  Number: TdomXPathNumberResult;
{$IFDEF VER140+}
  ExceptionMaskBackup: TFPUExceptionMask;
{$ENDIF}
begin
  if (left is TdomXPathMinusOperator) and (right is TdomXPathUnaryExpr) then begin
    Number:= XPathNumberFunc(TdomXPathUnaryExpr(right).evaluate(contextNode, contextPosition, contextSize));
    try
{$IFDEF VER140+}
      ExceptionMaskBackup:= SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$ENDIF}
      try
        result:= TdomXPathNumberResult.create(-(Number.asNumber));
{$IFDEF VER140+}
      finally
        SetExceptionMask(ExceptionMaskBackup);
{$ELSE}
      except
        result:= TdomXPathNumberResult.create(NaN);
{$ENDIF}
      end;
    finally
      Number.Free;
    end;
  end else if left is TdomXPathUnionExpr then begin
    result:= TdomXPathUnionExpr(left).evaluate(contextNode, contextPosition, contextSize);
  end else raise EXPath_Type_Err.create('XPath type error.');
end;

{ TdomXPathLiteral }

function TdomXPathLiteral.evaluate: TdomXPathCustomResult;
begin
  result:= TdomXPathStringResult.create(value);
end;

{ TdomXPathNumber }

function TdomXPathNumber.evaluate: TdomXPathCustomResult;
begin
  result:= TdomXPathNumberResult.create(XPathWideStringToNumber(value));
end;

{ TdomXPathVariableReference }

constructor TdomXPathVariableReference.create(const aOwner: TdomXPathSyntaxTree;
                                              const value: wideString);
begin
  inherited;
  FPrefix:= xmlExtractPrefix(value);
  FLocalName:= xmlExtractLocalName(value);
end;

function TdomXPathVariableReference.evaluate: TdomXPathCustomResult;
var
  NsUri: wideString;
begin
  result:= nil;

  // Determine Namespace URI
  NsUri:= lookupNamespaceURI;
  if (NsUri = '') and (FPrefix <> '') then
    raise ENamespace_Err.CreateFmt('Namespace URI of prefix ''%S'' not found.',
                                   [FPrefix]);

  if assigned(ownerSyntaxTree) then with ownerSyntaxTree do
    if assigned(contextNode) then with contextNode do
      if assigned(ownerDocument) then with ownerDocument do
        if assigned(domImplementation) then
          domImplementation.DoRequestXPathVariable(ownerXPathExpression, NsUri, FLocalName, result);

  if not assigned(result) then
    raise EXPath_Exception.CreateFmt('No binding for variable $%s provided.',[value]);
end;

function TdomXPathVariableReference.lookupNamespaceURI: wideString;
begin
  if assigned(ownerSyntaxTree) then begin
    if assigned(ownerSyntaxTree.contextNode)
      then Result := ownerSyntaxTree.contextNode.lookupNamespaceURI(FPrefix)
      else Result := '';
  end else Result := '';
end;

{ TdomXPathNameTest }

constructor TdomXPathNameTest.create(const AOwner: TdomXPathSyntaxTree;
                                     const value: wideString);
begin
  inherited;
  if value = '*' then begin
    FPrefix:= '';
    FLocalName:= '*';
  end else if value[length(value)] = '*' then begin
    FPrefix:= copy(value,1,length(value)-2);
    FLocalName:= '*';
  end else begin
    FPrefix:= xmlExtractPrefix(value);
    FLocalName:= xmlExtractLocalName(value);
  end;
end;

function TdomXPathNameTest.evaluate(const oldSnapshotResult: TdomXPathNodeSetResult;
                                     const principalNodeType: TdomNodeType): TdomXPathNodeSetResult;
var
  NsUri: wideString;
  i: integer;
begin
  if not assigned(oldSnapshotResult)
    then raise EXPath_Type_Err.create('XPath type error.');

  if value = '*' then begin

    with oldSnapshotResult do begin
      i:= pred(length);
      while i >= 0 do begin
        if item(i).nodeType <> principalNodeType then
          Delete(i);
        dec(i);
      end;
    end;

  end else begin

    // Determine Namespace URI
    NsUri := lookupNamespaceURI;
    if (NsUri = '') and (FPrefix <> '') then begin
      oldSnapshotResult.free;
      raise ENamespace_Err.CreateFmt('Namespace URI of prefix ''%S'' not found.',
                                     [FPrefix]);
    end;

    if FLocalName = '*' then begin

      with oldSnapshotResult do begin
        i:= pred(length);
        while i >= 0 do begin
          with item(i) do
            if (nodeType <> principalNodeType) or
               (namespaceURI <> NsUri) then
              Delete(i);
          dec(i);
        end;
      end;

    end else begin

      with oldSnapshotResult do begin
        i:= pred(length);
        while i >= 0 do begin
          with item(i) do
            if (nodeType <> principalNodeType) or
               (namespaceURI <> NsUri) or
               (localName <> FLocalName) then
              Delete(i);
          dec(i);
        end;
      end;

    end;

  end;

  result:= oldSnapshotResult;
end;

function TdomXPathNameTest.lookupNamespaceURI: wideString;
begin
  if assigned(ownerSyntaxTree) then begin
    if assigned(ownerSyntaxTree.contextNode)
      then Result := ownerSyntaxTree.contextNode.lookupNamespaceURI(FPrefix)
      else Result := '';
  end else Result := '';
end;

{ TdomXPathNodeTypeComment }

function TdomXPathNodeTypeComment.evaluate(const oldSnapshotResult: TdomXPathNodeSetResult): TdomXPathNodeSetResult;
var
  i: integer;
begin
  if not assigned(oldSnapshotResult)
    then raise EXPath_Type_Err.create('XPath type error.');

  with oldSnapshotResult do begin
    i:= pred(length);
    while i >= 0 do begin
      if item(i).nodeType <> ntComment_Node
        then Delete(i);
      dec(i);
    end;
  end;

  result:= oldSnapshotResult;
end;

{ TdomXPathNodeTypePI }

function TdomXPathNodeTypePI.evaluate(const oldSnapshotResult: TdomXPathNodeSetResult): TdomXPathNodeSetResult;
var
  i: integer;
begin
  if not assigned(oldSnapshotResult)
    then raise EXPath_Type_Err.create('XPath type error.');

  if assigned(left) then begin
    if left is TdomXPathLiteral then begin
      with oldSnapshotResult do begin
        i:= pred(length);
        while i >= 0 do begin
          with item(i) do
            if (nodeType <> ntProcessing_Instruction_Node) or
               (nodeValue <> TdomXPathLiteral(left).value)
              then Delete(i);
          dec(i);
        end;
      end;
    end else begin
      oldSnapshotResult.free;
      raise EXPath_Type_Err.create('XPath type error.');
    end;
  end else begin
    with oldSnapshotResult do begin
      i:= pred(length);
      while i >= 0 do begin
        if item(i).nodeType <> ntProcessing_Instruction_Node
          then Delete(i);
        dec(i);
      end;
    end;
  end;

  result:= oldSnapshotResult;
end;

{ TdomXPathNodeTypeText }

function TdomXPathNodeTypeText.evaluate(const oldSnapshotResult: TdomXPathNodeSetResult): TdomXPathNodeSetResult;
var
  i: integer;
begin
  if not assigned(oldSnapshotResult)
    then raise EXPath_Type_Err.create('XPath type error.');

  with oldSnapshotResult do begin
    i:= pred(length);
    while i >= 0 do begin
      if not (item(i).nodeType in [ntText_Node, ntCDATA_Section_Node, ntEntity_Reference_Node] )
        then Delete(i);
      dec(i);
    end;
  end;

  result:= oldSnapshotResult;
end;

end.
