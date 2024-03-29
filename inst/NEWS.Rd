% Check from R:
%  news(db = tools:::.build_news_db_from_package_NEWS_Rd("~/R/Pkgs/round/inst/NEWS.Rd"))
\name{NEWS}
\title{\pkg{round} R package  News}
\encoding{UTF-8}

\section{Changes in version 0.21-0.2 [2024-01-10]}{
  \subsection{Misc}{
    \itemize{
      \item 2nd try: \command{R CMD build} added the \file{*.R} to \code{inst/doc}, \dots
    }
  }
}

\section{Changes in version 0.21-0.1 [2023-12-07]}{
  \subsection{Misc}{
    \itemize{
      \item \command{R CMD build} now added the \file{*.R} to \code{inst/doc},
      circumventing \pkg{knitr}'s hack with \env{_R_CHECK_LICENCE_}.
    }
  }
}

\section{Changes in version 0.21-0 [2023-10-10]}{
  \subsection{BUG FIXES}{
    \itemize{
      \item \file{../tests/round-tst.R}: Better at detecting non-valid
      long-double; needed e.g., for BDR's valgrind instrumented R checks.

      \item R bugzilla URL updates.
    }
  }
}

\section{Changes in version 0.20-0 [2021-01-04]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item New vignette \file{rationalRound}, comparing CRAN package
      \CRANpkg{gmp}'s based exact rational rounding with our package's
      algorithms.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item adapt \file{tests/} to failing (NaN) for some \code{"r3.C"}
      cases, when there are no long doubles (as on Mac M1).
    }
  }
}

\section{Changes in version 0.12-2 [2020-07-04]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item in \code{roundX()}'s help page example, consider numerical noise
      when checking for equality of different versions.

      \item The \file{Rounding} vignette now gives \R 4.0.0's state and
      mentions mathematical \emph{exact rounding} via rational arithmetic.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Tweaked \file{tests/} slightly so they work on an \R configured
      with \command{--no-long-double}.
    }
  }
}

\section{Changes in version 0.12-1 [2020-01-14]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Added \code{trace} option to \code{"r3d.C"}, as it failed on
      32-bit (Windows and Fedora 30) platforms, giving \code{Inf} 7 out of
      16 times for \code{roundX(.Machine$double.xmax, -(295:310), "r3d.C")}.
    }
  }
}

\section{Changes in version 0.12-0 [2020-01-13]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Added rounding version \code{"r3d.C"}, version of \code{"r3.C"}
      which is \emph{not} using any \code{long double} and should match the
      pure \R version \code{"r3"} more closely.
    }
  }
}

\section{Changes in version 0.11-0 [2020-01-12]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item externalized \code{round_r3()} and added \code{info} option.

      \item provide simple wrapper \code{roundAll()} to apply all
      \code{roundVersions}.

      \item Obey CRAN requirements by adding info to \file{DESCRIPTION}, etc.

      \item Added C rounding version \code{"r3.C"}, by translating the \R
      code of \code{"r3"} to C, making use of \code{long double} to
      possibly get extended accuracy.
    }
  }
}

\section{Changes in version 0.10-1 [2020-01-06]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item added \code{"sprintf"} version for \code{round()}; new \code{roundVersions},  Jan. 2.
      \item + first regression tests.

      \item Added \R (interface to libc) version of C99 libmath functions \code{logB()}, \code{ilogb()},
      \code{nearbyint()}, \code{fpclassify()}, \code{isnormal()},
      \code{signbit()}, and \code{nextafter()}.

      \item added \code{"r2a.C"} version of \code{round()}, corresponding
      to my (svn r77618; Dec 24, 2019) commit to R-devel.

      \item added \code{"r3"}, my \dQuote{optimal} \code{round()} pure \R version.
      \item new \code{randI()} for random integers of specified number of
      digits, in order to create simulated data examples for
      \code{round*()} testing.
    }
  }
}

\section{Changes in version 0.10-0 [2020-01-01]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item New package \pkg{round}: \emph{Implements several versions of
	\code{round(x, digits)}};
      with just the one function \code{roundX(x, digits, version)} and
      possible versions \code{c("r0", "r1", "r1a", "r2")}, all via C code
      of current (R-devel) and former (R and R-devel) versions of \R's own
      \code{round(x, digits)}.

      E.g., \code{"r2"} is related to my change of \code{round()} in
      R-devel via svn commit r77618, on 2019-12-24, with message
      \emph{tweaks to round() bug fix for PR#17668 in r77609}.
    }
  }
}
