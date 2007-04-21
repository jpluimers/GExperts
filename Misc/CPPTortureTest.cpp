
//These all work fine:
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
 return 1;
}
//---------------------------------------------------------------------------

int WINAPI DllEntryPoint2(HINSTANCE hinst,
 unsigned long reason, //test
  void*)
{
 return 1;
}

void TestFunction()
{}

void TestFunction2(){}


void Function1()
{
  if (Function2(fred, bert)){
     SomeOtherOperations().....
  }
}

void xList::Exchange(int index1, int index2) throw(xException)
{
  void * obj;
  if (FSorted)
    throw xException(DEBUGINFO,"Exchange in a sorted list not allowed");
}


int myfunction(void (*pFunction) ()) { }

#include "Unit1.h"

void DummyFunc()
{
}

int myfunction(type *aa, type2 *bb,  void (*pFunction) (), int tag)
{
}

int AFunction(int x)
{
  return x;
}

void pete0()
{
}
void pete1()
{
#if 1
  pete0();
#endif
}
int pete2(void)
{
  return 0;
}

Singleton & Singleton::instance()
{
   static Singleton * singleton = NULL;
   if (!singleton) singleton = new Singleton;
   return *singleton;
}

int someclass::init()
{
 // blah
}

static S1 const t1[] = {
   { "abc", 123 },
   { "def", 456 }
};

static S2 const s2 = { "abc", 123 };
static A const a[] = {
   "abc", "def", A( "ghi" )
};

struct s {
    char * a[];
};


static struct s b = { {"bla", "blu"} };

struct qmap {
   char * group;
};

int
main(argc, argv)
{
   struct qmap qtable = {
#ifdef TRIPBUG
     {"summer",  2},
#endif /* TRIPBUG */
     {(char *)0, 0}
   };
}

// Properly ignored?
int AFunction(int x//) missing
{ return x; }

// Old C style: not found
void error_message(hint)
   char *hint;
{
}


// Regression: Named TForm?
__fastcall TFBoxForm::TFBoxForm(TComponent* Owner)
        : TForm(Owner)
{
  Blah;
}

// Regression: Not recognized?
#ifdef  PRONTODEBUGGER
__declspec(naked) void    DEFCC   _ThrowExceptionLDTC(void    __far * tpid,
				void    __far * throwAddr,
				void          * errPtr)
#endif
{
  //test
}

// Regression: Marked as a constructor?
static  void    destroyOneObject(void   __far * varAddr,
				 tpidPtr        varType,
#ifdef  NEWXX
				 unsigned       flags,
#endif
				 PREGREC_BC     errPtr)
{
  assert(0);
}

// Old Bug: Only finds XOleInPlaceActiveObject::EnableModeless?
STDMETHODIMP COleServerDoc::XOleInPlaceActiveObject::EnableModeless(
	BOOL fEnable)
{
	return sc;
}

// Finds x:CATCH_ALL below (maybe not fixable?, macros?, what is x: from?)
LRESULT CALLBACK
_AfxActivationWndProc(HWND hWnd, UINT nMsg, WPARAM wParam, LPARAM lParam)
{
	TRY
	{
	}
	CATCH_ALL(e)
	{
	}
	END_CATCH_ALL
}

BOOL CFrameWnd::NegotiateBorderSpace(UINT nBorderCmd, LPRECT lpRectBorder)
{
	CRect border, request;

	switch (nBorderCmd)
	{
	case borderGet:
		ASSERT(lpRectBorder != NULL);
		RepositionBars(0, 0xffff, AFX_IDW_PANE_FIRST, reposQuery,
			lpRectBorder);
		break;

	case borderRequest:
		return TRUE;

	case borderSet:
		if (lpRectBorder == NULL)
		{
			if (!m_rectBorder.IsRectNull())
			{
				// releasing all border space -- recalc needed
				m_rectBorder.SetRectEmpty();
				return TRUE;
			}
			// original rect is empty & lpRectBorder is NULL, no recalc needed
			return FALSE;
		}
		if (!::EqualRect(m_rectBorder, lpRectBorder))
		{
			// the rects are different -- recalc needed
			m_rectBorder.CopyRect(lpRectBorder);
			return TRUE;
		}
		return FALSE;   // no recalc needed

	default:
		ASSERT(FALSE);  // invalid CFrameWnd::BorderCmd
	}

	return TRUE;
}

struct __declspec(novtable) __declspec(dllexport) test6
{
void b() {};
};

class _afxRichEditCookie
{
public:
	CArchive& m_ar;
	DWORD m_dwError;
	_afxRichEditCookie(CArchive& ar) : m_ar(ar) {m_dwError=0;}
};


template <class Base, const IID* piid, class T, class Copy>
STDMETHODIMP CComEnumImpl<Base, piid, T, Copy>::Skip(ULONG celt)
{
    m_iter += celt;
    if (m_iter >= m_end)
	{
        m_iter = m_end;
        return S_FALSE;
    }
    if (m_iter < m_begin)
	{	
        m_iter = m_begin;
        return S_FALSE;
    }
    return S_OK;
}

class ATL_NO_VTABLE MyClass : public FredFred {
private:
  int fVal;
public:
  MyClass() : fVal(0) {}
  ~MyClass() {}
  int MyFunc(int x) {
    x = 4;
  }
  static void DoItNow() {}
};			



BOOL CFrameWnd::NegotiateBorderSpace(UINT nBorderCmd)
{
switch (nBorderCmd)
 {
 case borderGet:
  RepositionBars(0);
 case borderSet:
  if (lpRectBorder == NULL){}
 }
}


void test() {
	default:
		pFX->Default(szName, &value, plLength, SQL_TINYINT,
			sizeof(value), 3);
		break;

	case CFieldExchange::Fixup:
		if (*plLength == SQL_NULL_DATA){ }
}


void test2() {
	case CFieldExchange::Value:
		if (pFX->m_prs->IsFieldStatusDirty(nField - 1))
		{
			// If optimizing for bulk add, only need lengths & proxy set correctly
			if(!(pFX->m_prs->m_dwOptions & CRecordset::optimizeBulkAdd))
			{
				AFX_SQL_SYNC(::SQLBindParameter(pFX->m_hstmt, (UWORD)pFX->m_nParamFields));
			}
		}
		return;

	case CFieldExchange::SetFieldNull:
		if ((pFX->m_pvField == NULL && False
			pFX->m_nFieldType == CFieldExchange::outputColumn) ||
			pFX->m_pvField == &value){ }
		return;
}

void CFieldExchange::Default(LPCTSTR szName,
	void* pv, LONG* plLength, int nCType, UINT cbValue, UINT cbPrecision)
{
	case RebindParam:
		// Only need to reset param length
		*plLength = m_prs->IsParamStatusNull(nField - 1) ? SQL_NULL_DATA : cbValue;

	case BindFieldForUpdate:
		if (false)
		{
		}
}

BOOL AFXAPI AfxIsDescendant(HWND hWndParent, HWND hWndChild)
	// helper for detecting whether child descendent of parent
	//  (works with owned popups as well)
{
	ASSERT(::IsWindow(hWndParent));
	return FALSE;
}

 #if 0
static void FOC_spinner_task(void)
{
  while (1)
  {
     for (j = 0; j < 14; j++)
    {
      if (string_pos == 7)
        adder = -1;
        string_pos += adder;
     }
  }
}
#endif
/******************************************************************************
  Overview:     Function to .
  Parameters:
  Description:  .
 ******************************************************************************/

void FOC_control_foc(UBYTE ctrl_reg, UBYTE ctrl_data)
{
}

// These type were not previously found, but now can cause infinite loops at the end of files
namespace Msxml3_srvr
{
// Causes infinite loop, if you // comment out the last line in this file
void __fastcall PACKAGE Register()
{
  // [26]
  TComponentClass cls_svr[] = {
                              __classid(Msxml3::TmsDOMDocument),
                              __classid(Msxml3::TmsSAXAttributes30)
                           };
  RegisterComponents("XML3", cls_svr,
                     sizeof(cls_svr)/sizeof(cls_svr[0])-1);
}

};     // namespace Msxml3_srvr

//int test{call();}

// ---------------------------------
// Remaining Bugs:
// ---------------------------------

// Empty namespaces cause the nemspace not to get popped off the stack
struct test5 {
// int a;
};
void failed_test(){}
