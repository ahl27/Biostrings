/****************************************************************************
 *  Low-level manipulation of BString, BStringList and BStringSet objects   *
 *                           Author: Herve Pages                            *
 ****************************************************************************/
#include "Biostrings.h"


/****************************************************************************
 * Encoding/decoding BString data.
 */

static int DNA_enc_lkup[256], DNA_dec_lkup[256];
static int RNA_enc_lkup[256], RNA_dec_lkup[256];

static void copy_lkup(const int *lkup1, int len1, int *lkup2, int len2)
{
	int i;

	if (len1 > len2)
		error("Biostrings internal error in copy_lkup(): len1 > len2");
	for (i = 0; i < len1; i++)
		lkup2[i] = lkup1[i];
	for ( ; i < len2; i++)
		lkup2[i] = NA_INTEGER;
}

static int class2code(const char *class)
{
	error("class2code() not ready yet");
	return 0;
}

static const char *code2class(int code)
{
	error("code2class() not ready yet");
	return 0;
}

SEXP init_DNAlkups(SEXP enc_lkup, SEXP dec_lkup)
{
	copy_lkup(INTEGER(enc_lkup), LENGTH(enc_lkup),
		DNA_enc_lkup, sizeof(DNA_enc_lkup) / sizeof(int));
	copy_lkup(INTEGER(dec_lkup), LENGTH(dec_lkup),
		DNA_dec_lkup, sizeof(DNA_dec_lkup) / sizeof(int));
	return R_NilValue;
}

char _DNAencode(char c)
{
	int code;

	code = DNA_enc_lkup[(unsigned char) c];
	if (code == NA_INTEGER)
		error("_DNAencode: key %d not in lookup table", (int) c);
	return code;
}

char _DNAdecode(char code)
{
	int c;

	c = DNA_dec_lkup[(unsigned char) code];
	if (c == NA_INTEGER)
		error("_DNAdecode: key %d not in lookup table", (int) code);
	return c;
}

SEXP init_RNAlkups(SEXP enc_lkup, SEXP dec_lkup)
{
	copy_lkup(INTEGER(enc_lkup), LENGTH(enc_lkup),
		RNA_enc_lkup, sizeof(RNA_enc_lkup) / sizeof(int));
	copy_lkup(INTEGER(dec_lkup), LENGTH(dec_lkup),
		RNA_dec_lkup, sizeof(RNA_dec_lkup) / sizeof(int));
	return R_NilValue;
}

char _RNAencode(char c)
{
	int code;

	code = RNA_enc_lkup[(unsigned char) c];
	if (code == NA_INTEGER)
		error("_RNAencode: key %d not in lookup table", (int) c);
	return code;
}

char _RNAdecode(char code)
{
	int c;

	c = RNA_dec_lkup[(unsigned char) code];
	if (c == NA_INTEGER)
		error("_RNAdecode: key %d not in lookup table", (int) code);
	return (char) c;
}


/****************************************************************************
 * Low-level manipulation of BString objects.
 */

const char *get_class(SEXP x)
{
	return CHAR(STRING_ELT(GET_CLASS(x), 0));
}

static SEXP getBString_data(SEXP x)
{
	return GET_SLOT(x, install("data"));
}

const char *_get_BString_charseq(SEXP x, int *length)
{
	SEXP xp;
	int offset;

	xp = GET_SLOT(getBString_data(x), install("xp"));
	offset = INTEGER(GET_SLOT(x, install("offset")))[0];
	*length = INTEGER(GET_SLOT(x, install("length")))[0];
	return (const char *) (RAW(R_ExternalPtrTag(xp)) + offset);
}

/* UNTESTED */
/* NOT a Call() entry point! */
SEXP mkBString(const char *class, SEXP data, int offset, int length)
{
	SEXP class_def, ans;

	class_def = MAKE_CLASS(class);
	PROTECT(ans = NEW_OBJECT(class_def));
	SET_SLOT(ans, mkChar("data"), data);
	SET_SLOT(ans, mkChar("offset"), ScalarInteger(offset));
	SET_SLOT(ans, mkChar("length"), ScalarInteger(length));
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * Low-level manipulation of BStringList objects.
 */

int _get_BStringList_length(SEXP x)
{
	return LENGTH(GET_SLOT(x, install("seqs")));
}

const char *_get_BStringList_charseq(SEXP x, int i, int *nchar)
{
	SEXP seqs, seq;

	seqs = GET_SLOT(x, install("seqs"));
	seq = VECTOR_ELT(seqs, i);
	return _get_BString_charseq(seq, nchar);
}

/* 'x_seqs' must be the list, NOT the BStringList object!
 * TODO: make this work directly on the BStringList object and use the
 * 2 helper functions above to simplify the code.
 */
SEXP BStrings_to_nchars(SEXP x_seqs)
{
	SEXP ans, x_seq;
	int nseq, i, *seq_length;

	nseq = LENGTH(x_seqs);
	PROTECT(ans = NEW_INTEGER(nseq));
	for (i = 0, seq_length = INTEGER(ans); i < nseq; i++, seq_length++) {
		x_seq = VECTOR_ELT(x_seqs, i);
		_get_BString_charseq(x_seq, seq_length);
	}
	UNPROTECT(1);
	return ans;
}


/****************************************************************************
 * Low-level manipulation of BStringSet objects.
 */

int _get_BStringSet_length(SEXP x)
{
	SEXP inters;

	inters = GET_SLOT(x, install("inters"));
	return LENGTH(VECTOR_ELT(inters, 0));
}

const char *_get_BStringSet_charseq(SEXP x, int i, int *nchar)
{
	SEXP inters, super, xp;
	int start, offset;

	inters = GET_SLOT(x, install("inters"));
	start = INTEGER(VECTOR_ELT(inters, 0))[i];
	*nchar = INTEGER(VECTOR_ELT(inters, 1))[i];
	super = GET_SLOT(x, install("super"));
	xp = GET_SLOT(getBString_data(super), install("xp"));
	offset = INTEGER(GET_SLOT(super, install("offset")))[0];
	return (const char *) (RAW(R_ExternalPtrTag(xp)) + offset + start - 1);
}

