options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode
golem::detach_all_attached()
golem::document_and_reload()
run_app()