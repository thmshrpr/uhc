%include lhs2TeX.fmt
%include afp.fmt
%include ehrules.fmt

\rulerCmdDef{rules.E.expr.base.scheme}{%
\ensuremath{| Gamma :- | ^{ | e | } |  e : sigma | }
}
\rulerCmdDef{rules.E.expr.base.e.int}{%
\rulerRule{e.int}{E}
{%
}
{%
| Gamma :- | ^{ | e | } |  int : Int | 
}
}

\rulerCmdDef{rules.E.expr.base.e.char}{%
\rulerRule{e.char}{E}
{%
}
{%
| Gamma :- | ^{ | e | } |  char : Char | 
}
}

\rulerCmdDef{rules.E.expr.base.e.str}{%
\rulerRule{e.str}{E}
{%
}
{%
| Gamma :- | ^{ | e | } |  str : tyString | 
}
}

\rulerCmdDef{rules.E.expr.base.e.float}{%
\rulerRule{e.float}{E}
{%
}
{%
| Gamma :- | ^{ | e | } |  float : tyFloat | 
}
}

\rulerCmdDef{rules.E.expr.base.e.iint}{%
\rulerRule{e.iint}{E}
{%
}
{%
| Gamma :- | ^{ | e | } |  integer : tyInteger | 
}
}

\rulerCmdDef{rules.E.expr.base.e.double}{%
\rulerRule{e.double}{E}
{%
}
{%
| Gamma :- | ^{ | e | } |  double : tyDouble | 
}
}

\rulerCmdDef{rules.E.expr.base.e.var}{%
\rulerRule{e.var}{E}
{%
| ident :-> sigma `elem` Gamma | 
}
{%
| Gamma :- | ^{ | e | } |  ident : sigma | 
}
}

\rulerCmdDef{rules.E.expr.base.e.app}{%
\rulerRule{e.app}{E}
{%
| Gamma :- | ^{ | e | } |  e | _{ | 2 | } |  : sigma | _{ | a | } |  | 
\\
| Gamma :- | ^{ | e | } |  e | _{ | 1 | } |  : sigma | _{ | a | } |  -> sigma | 
}
{%
| Gamma :- | ^{ | e | } |  e | _{ | 1 | } |  ^^ e | _{ | 2 | } |  : sigma | 
}
}

\rulerCmdDef{rules.E.expr.base.e.lam}{%
\rulerRule{e.lam}{E}
{%
| (identv :-> sigma | _{ | identv | } | ) + Gamma :- | ^{ | e | } |  e : sigma | _{ | e | } |  | 
}
{%
| Gamma :- | ^{ | e | } |   \ identv -> e : sigma | _{ | identv | } |  -> sigma | _{ | e | } |  | 
}
}

\rulerCmdDef{rules.E.expr.base.e.let}{%
\rulerRule{e.let}{E}
{%
| Gamma | _{ | t | } |  ++ Gamma :- | ^{ | d | } |  d : Gamma | _{ | t | } |  | 
\\
| Gamma | _{ | t | } |  ++ Gamma :- | ^{ | e | } |  b : sigma | 
}
{%
| Gamma :- | ^{ | e | } |  let d in b : sigma | 
}
}

\rulerCmdDef{rules.E.expr.base.e.ann}{%
\rulerRule{e.ann}{E}
{%
|  :- | ^{ | t | } |  t : sigma | 
\\
| Gamma :- | ^{ | e | } |  e : sigma | 
}
{%
| Gamma :- | ^{ | e | } |  (e :: t) : sigma | 
}
}

\rulerCmdDef{rules.E.expr.base.e.prod}{%
\rulerRule{e.prod}{E}
{%
| Gamma :- | ^{ | e | } |  e | _{ | 1 | } |  : sigma | _{ | 1 | } |  | 
\\
| Gamma :- | ^{ | e | } |  e | _{ | 2 | } |  : sigma | _{ | 2 | } |  | 
}
{%
| Gamma :- | ^{ | e | } |  (e | _{ | 1 | } |  , e | _{ | 2 | } | ) : (sigma | _{ | 1 | } |  , sigma | _{ | 2 | } | ) | 
}
}

\rulerCmdDef{rules.E.expr.base}{%
\begin{rulerRulesetFigure}{\rulerCmdUse{rules.E.expr.base.scheme}}{Expression type rules}{rules.E.expr.base}{E}
\rulerCmdUse{rules.E.expr.base.e.int}
\hspace{1ex}
\rulerCmdUse{rules.E.expr.base.e.char}
\hspace{1ex}
\rulerCmdUse{rules.E.expr.base.e.str}
\hspace{1ex}
\rulerCmdUse{rules.E.expr.base.e.float}
\hspace{1ex}
\rulerCmdUse{rules.E.expr.base.e.iint}
\hspace{1ex}
\rulerCmdUse{rules.E.expr.base.e.double}
\hspace{1ex}
\rulerCmdUse{rules.E.expr.base.e.var}
\hspace{1ex}
\rulerCmdUse{rules.E.expr.base.e.app}
\hspace{1ex}
\rulerCmdUse{rules.E.expr.base.e.lam}
\hspace{1ex}
\rulerCmdUse{rules.E.expr.base.e.let}
\hspace{1ex}
\rulerCmdUse{rules.E.expr.base.e.ann}
\hspace{1ex}
\rulerCmdUse{rules.E.expr.base.e.prod}
\end{rulerRulesetFigure}
}

\rulerCmdDef{rules.E.expr.baseForEH1.scheme}{%
\ensuremath{| Gamma :- | ^{ | e | } |  e : sigma | }
}
\rulerCmdDef{rules.E.expr.baseForEH1.e.int}{%
\rulerCmdUse{rules.E.expr.base.e.int}
}

\rulerCmdDef{rules.E.expr.baseForEH1.e.char}{%
\rulerCmdUse{rules.E.expr.base.e.char}
}

\rulerCmdDef{rules.E.expr.baseForEH1.e.var}{%
\rulerCmdUse{rules.E.expr.base.e.var}
}

\rulerCmdDef{rules.E.expr.baseForEH1.e.app}{%
\rulerCmdUse{rules.E.expr.base.e.app}
}

\rulerCmdDef{rules.E.expr.baseForEH1.e.lam}{%
\rulerCmdUse{rules.E.expr.base.e.lam}
}

\rulerCmdDef{rules.E.expr.baseForEH1.e.ann}{%
\rulerCmdUse{rules.E.expr.base.e.ann}
}

\rulerCmdDef{rules.E.expr.baseForEH1.e.let}{%
\rulerCmdUse{rules.E.expr.base.e.let}
}

\rulerCmdDef{rules.E.expr.baseForEH1}{%
\begin{rulerRulesetFigure}{\rulerCmdUse{rules.E.expr.baseForEH1.scheme}}{Expression type rules}{rules.E.expr.baseForEH1}{E}
\rulerCmdUse{rules.E.expr.baseForEH1.e.int}
\hspace{1ex}
\rulerCmdUse{rules.E.expr.baseForEH1.e.char}
\hspace{1ex}
\rulerCmdUse{rules.E.expr.baseForEH1.e.var}
\hspace{1ex}
\rulerCmdUse{rules.E.expr.baseForEH1.e.app}
\hspace{1ex}
\rulerCmdUse{rules.E.expr.baseForEH1.e.lam}
\hspace{1ex}
\rulerCmdUse{rules.E.expr.baseForEH1.e.ann}
\hspace{1ex}
\rulerCmdUse{rules.E.expr.baseForEH1.e.let}
\end{rulerRulesetFigure}
}

\rulerCmdDef{rules.E.expr.scratch.scheme}{%
\ensuremath{| Gamma :- | ^{ | e | } |  e : sigma | }
}
\rulerCmdDef{rules.E.expr.scratch.e.let}{%
\rulerCmdUse{rules.E.expr.base.e.let}
}

\rulerCmdDef{rules.E.expr.scratch}{%
\begin{rulerRulesetFigure}{\rulerCmdUse{rules.E.expr.scratch.scheme}}{Expression type rules (scratch)}{rules.E.expr.scratch}{E}
\rulerCmdUse{rules.E.expr.scratch.e.let}
\end{rulerRulesetFigure}
}

