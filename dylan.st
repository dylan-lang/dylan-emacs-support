/**
 * Name: dylan
 * Description: Dylan Programming Language template for Enscript.
 * Author: Brent Fulgham <bfulgham@debian.org>
 * (based on the Scheme version by Markku Rossi <mtr@iki.fi>)
 */

dylan_mod_re =
/* Definition Modifiers
  (build-re '(abstract block concrete constant class domain exception exclude
  export function functional generic handler import inherited inline
  instance interface library macro method open primary sealed sideways
  singleton slot subclass variable virtual))
  */
  /\b(subclass|abstract|block|c(on(crete|stant)|lass)|domain\
|ex(c(eption|lude)|port)|f(unction(|al))|generic|handler\
|i(n(herited|line|stance|terface)|mport)|library|m(acro|ethod)\
|open|primary|sealed|si(deways|ngleton)|slot\
|v(ariable|irtual))\b/;

state dylan extends HighlightEntry
{
  BEGIN {
    /*
     * Modify regexp character syntax so that we can distinguish all
     * dylan symbols.
     */
    extras = list ('!', '$', '%', '&', '*', '/', ':', 
		   '=', '?', '~', '^', '.', '+', '-');
    for (i = 0; i < length (extras); i = i + 1)
      regexp_syntax (extras[i], 'w');
  }

  /* Modifiers */
  dylan_mod_re {
    reference_face (true);
    language_print ($0);
    reference_face (false);
  }

  /* Types */
  /<\w+>/ {
    type_face (true);
    language_print ($0);
    type_face (false);
  }

  /* Symbols */
  /#\"/ {
    string_face (true);
    language_print ($0);
    call (c_string);
    string_face (false);
  }

  /* Comments. */
  /\/\// {
    comment_face (true);
    language_print ($0);
    call (eat_one_line);
    comment_face (false);
  }
  /\/\*/ {
    comment_face (true);
    language_print ($0);
    call (eat_one_line);
    comment_face (false);
  }

  /* String constants. */
  /\"/ {
    string_face (true);
    language_print ($0);
    call (c_string);
    string_face (false);
  }

  /* Character constants. */
  /'.*'/ {
    string_face (true);
    language_print ($0);
    string_face (false);
  }
    
  /* Keywords.
     "=>" +
     (build-re '(above afterwards begin below by case cleanup create
     define end else elseif finally for from if in let local otherwise rename
     select signal then to unless until use variable virtual when while
   */
  /=>|\b(a(bove|fterwards)|b(e(gin|low)|y)|c(ase|leanup|reate)\
|define|else(|if)|end|f(inally|or|rom)|i[fn]|l(et|ocal)|otherwise\
|rename|s(elect|ignal)|t(hen|o)|u(n(less|til)|se)|wh(en|ile))\b/ {
    keyword_face (true);
    language_print ($0);
    keyword_face (false);
  }

  /* ':'-names, Emacs highlights these, so do we. */
  /([ \t])([!\$%&\*\/:<=>\?~_^a-zA-Z0-9.+\-]*:)/ {
    language_print ($1);
    reference_face (true);
    language_print ($2);
    reference_face (false);
  }

  /* Function faces */
  /([ \t]*)(\w+[^:])([ \t]*\([ \t]*)/ {
    language_print ($1);
    function_name_face(true);
    face_on(face_bold_italic);
    language_print ($2);
    face_off(face_bold_italic);
    function_name_face(false);
    language_print ($3);
  }
}


/*
Local variables:
mode: c
End:
*/
