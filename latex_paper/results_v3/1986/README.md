# V3 forecast reporting

Regenerate from the ML_RV project with source('R/report_results.R'). No estimation is performed.
The project R library must be available. The script stops on inconsistent inputs.

Compile from latex_paper: pdflatex -interaction=nonstopmode -halt-on-error -output-directory=results_v3/1986 results_v3/1986/results_preview.tex

Integration: manuscript.tex reads main.tex, main_appendix.tex and internet_appendix.tex from this directory. The snippets require graphicx and longtable.
See notes.tex for units, aggregation, validation, ensemble and surrogate conventions.
data/ contains the EW RDS objects, forecasts, all metrics, tuning choices, feature coefficients and source hashes.
main/, main_appendix/ and internet_appendix/ contain generated LaTeX tables and figure wrappers plus PDF/PNG figures.
Only this generated results_v3/1986 directory is overwritten on rerun. Existing manuscript and estimation artifacts are not changed.
