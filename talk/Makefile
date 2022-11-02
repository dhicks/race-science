all: talk

talk: talk.html
talk.html: talk.qmd ../paper/race_science.yaml custom.scss
	quarto render talk.qmd

publish:
	quarto publish netlify --id cf96094d-4346-4f6f-afd2-d2228bab4b59
