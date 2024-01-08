#ifndef OAIDL_HEADER
#define OAIDL_HEADER

// https://github.com/tpn/winsdk-10/blob/master/Include/10.0.16299.0/um/OAIdl.h

#include <windows.h>

#define DISPATCH_METHOD 0x1
#define DISPATCH_PROPERTYGET 0x2
#define DISPATCH_PROPERTYPUT 0x4
#define DISPATCH_PROPERTYPUTREF 0x8
#define DISPID_NEWENUM -4

#define S_OK 0x00
#define S_FALSE 0x01
#define E_NOT_IMPLEMENTED 0x80004001
#define E_NOTIMPL 0x80004001
#define E_NO_INTERFACE 0x80004002
#define E_NOINTERFACE 0x80004002
#define E_POINTER 0x80004003
#define E_UNEXPECTED 0x8000FFFF

#define DISP_E_MEMBERNOTFOUND 0x80020003
#define DISP_E_PARAMNOTOPTIONAL 0x80020004
#define DISP_E_UNKNOWNNAME 0x80020006
#define DISP_E_BADVARTYPE 0x80020008
#define DISP_E_BADPARAMCOUNT 0x8002000E
#define DISP_E_EXCEPTION 0x80020009

// https://github.com/tpn/winsdk-10/blob/master/Include/10.0.16299.0/shared/wtypes.h
#define VT_EMPTY 0
#define VT_NULL 1
#define VT_I4 3
#define VT_R4 4
#define VT_R8 5
#define VT_BSTR 8
#define VT_DISPATCH 9
#define VT_BOOL 11
#define VT_VARIANT 12
#define VT_UNKNOWN 13
#define VT_I8 20
#define VT_BYREF 16384

typedef struct IDispatch IDispatch;
typedef void *IUnknown;

// guiddef.h
// https://docs.microsoft.com/en-us/windows/win32/api/guiddef/ns-guiddef-guid
typedef struct _GUID
{
	unsigned long Data1;
	unsigned short Data2;
	unsigned short Data3;
	unsigned char Data4[8];
} GUID;

// https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-oaut/94ea3f27-ba29-45f3-a821-323f0a0c6d60
typedef GUID IID;

// https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-oaut/bbde795f-5398-42d8-9f59-3613da03c318
typedef IID *REFIID;

// https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-oaut/b2c46afb-4717-4cbb-8828-d6e0ae743463
typedef WCHAR *LPOLESTR;

// https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-oaut/b0b43e39-b080-4edd-a26d-7134075c75cd
typedef LONG DISPID;
#define DISPID_VALUE 0
#define DISPID_UNKNOWN -1
#define DISPID_PROPERTYPUT -3
#define DISPID_NEWENUM -4

// Just trust me on this one
typedef short VARTYPE;
typedef double DOUBLE;

// This one's only kind of correct
typedef short *BSTR;

// https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-oaut/7b39eb24-9d39-498a-bcd8-75c38e5823d0
typedef short VARIANT_BOOL;
#define VARIANT_TRUE 0xFFFF
#define VARIANT_FALSE 0x0000

// https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-oaut/b5493025-e447-4109-93a8-ac29c48d018d
typedef struct tagDEC
{
	WORD wReserved;
	BYTE scale;
	BYTE sign;
	ULONG Hi32;
	ULONGLONG Lo64;
} DECIMAL;

// https://docs.microsoft.com/en-us/windows/win32/api/oaidl/ns-oaidl-variant
#define __VARIANT_NAME_1
#define __VARIANT_NAME_2
#define __VARIANT_NAME_3
#define __VARIANT_NAME_4
typedef struct tagVARIANT
{
	union
	{
		struct
		{
			VARTYPE vt;
			WORD wReserved1;
			WORD wReserved2;
			WORD wReserved3;
			union
			{
				LONGLONG llVal;
				LONG lVal;
				BYTE bVal;
				SHORT iVal;
				FLOAT fltVal;
				DOUBLE dblVal;
				VARIANT_BOOL boolVal;
				VARIANT_BOOL __OBSOLETE__VARIANT_BOOL;
				// SCODE        scode;
				// CY           cyVal;
				// DATE         date;
				BSTR bstrVal;
				IUnknown *punkVal;
				IDispatch *pdispVal;
				// SAFEARRAY    *parray;
				BYTE *pbVal;
				SHORT *piVal;
				LONG *plVal;
				LONGLONG *pllVal;
				FLOAT *pfltVal;
				DOUBLE *pdblVal;
				VARIANT_BOOL *pboolVal;
				VARIANT_BOOL *__OBSOLETE__VARIANT_PBOOL;
				// SCODE        *pscode;
				// CY           *pcyVal;
				// DATE         *pdate;
				BSTR *pbstrVal;
				IUnknown **ppunkVal;
				IDispatch **ppdispVal;
				// SAFEARRAY    **pparray;
				struct tagVARIANT *pvarVal;
				PVOID byref;
				CHAR cVal;
				USHORT uiVal;
				ULONG ulVal;
				ULONGLONG ullVal;
				INT intVal;
				UINT uintVal;
				DECIMAL *pdecVal;
				CHAR *pcVal;
				USHORT *puiVal;
				ULONG *pulVal;
				ULONGLONG *pullVal;
				INT *pintVal;
				UINT *puintVal;
				struct
				{
					PVOID pvRecord;
					PVOID pRecInfo; // IRecordInfo *pRecInfo;
				} __VARIANT_NAME_4;
			} __VARIANT_NAME_3;
		} __VARIANT_NAME_2;
		DECIMAL decVal;
	} __VARIANT_NAME_1;
} VARIANT;

// https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-oaut/144b00dd-4c2f-4b35-a28f-c17f591b990c
typedef struct tagDISPPARAMS
{
	VARIANT *rgvarg;
	DISPID *rgdispidNamedArgs;
	UINT cArgs;
	UINT cNamedArgs;
} DISPPARAMS;

// https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-oaut/a7bb989f-5c55-49c7-98e1-24ab2593a9fa
typedef struct tagEXCEPINFO
{
	WORD wCode;
	WORD wReserved;
	BSTR bstrSource;
	BSTR bstrDescription;
	BSTR bstrHelpFile;
	DWORD dwHelpContext;
	ULONG_PTR pvReserved;
	ULONG_PTR pfnDeferredFillIn;
	HRESULT scode;
} EXCEPINFO;

typedef struct IDispatchVtbl
{
	__stdcall HRESULT(*QueryInterface)
	(
		IDispatch *This,
		/* [in] */ REFIID riid,
		/* [out] */ void **ppvObject);

	__stdcall ULONG(*AddRef)
	(
		IDispatch *This);

	__stdcall ULONG(*Release)
	(
		IDispatch *This);

	__stdcall HRESULT(*GetTypeInfoCount)
	(
		IDispatch *This,
		/* [out] */ UINT *pctinfo);

	__stdcall HRESULT(*GetTypeInfo)(
		IDispatch * This,
		/* [in] */ UINT iTInfo,
		/* [in] */ LCID lcid,
		// /* [out] */ ITypeInfo **ppTInfo);
		/* [out] */ void **ppTInfo);

	__stdcall HRESULT(*GetIDsOfNames)
	(
		IDispatch *This,
		/* [in] */ REFIID riid,
		/* [in] */ LPOLESTR *rgszNames,
		/* [in] */ UINT cNames,
		/* [in] */ LCID lcid,
		/* [out] */ DISPID *rgDispId);

	__stdcall HRESULT(*Invoke)
	(
		IDispatch *This,
		/* [in] */ DISPID dispIdMember,
		/* [in] */ REFIID riid,
		/* [in] */ LCID lcid,
		/* [in] */ WORD wFlags,
		/* [out][in] */ DISPPARAMS *pDispParams,
		/* [out][opt] */ VARIANT *pVarResult,
		/* [out][opt] */ EXCEPINFO *pExcepInfo,
		/* [out][opt] */ UINT *puArgErr);

} IDispatchVtbl;

typedef struct IDispatch
{
	IDispatchVtbl *lpVtbl;
} IDispatch;

typedef struct IEnumVARIANT IEnumVARIANT;

typedef struct IEnumVARIANTVtbl
{
	__stdcall HRESULT(*QueryInterface)
	(
		IEnumVARIANT *This,
		/* [in] */ REFIID riid,
		/* [out] */ void **ppvObject);

	__stdcall ULONG(*AddRef)
	(
		IEnumVARIANT *This);

	__stdcall ULONG(*Release)
	(
		IEnumVARIANT *This);

	__stdcall HRESULT(*Next)
	(
		IEnumVARIANT *This,
		/* [in] */ ULONG celt,
		/* [out] */ VARIANT *rgVar,
		/* [out] */ ULONG *pCeltFetched);

	__stdcall HRESULT(*Skip)
	(
		IEnumVARIANT *This,
		/* [in] */ ULONG celt);

	__stdcall HRESULT(*Reset)
	(
		IEnumVARIANT *This);

	__stdcall HRESULT(*Clone)
	(
		IEnumVARIANT *This,
		/* [out] */ IEnumVARIANT **ppEnum);

} IEnumVARIANTVtbl;

struct IEnumVARIANT
{
	IEnumVARIANTVtbl *lpVtbl;
};

#endif // OAIDL_HEADER