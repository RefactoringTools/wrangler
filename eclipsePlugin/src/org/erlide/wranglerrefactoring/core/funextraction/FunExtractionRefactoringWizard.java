package org.erlide.wranglerrefactoring.core.funextraction;

import org.erlide.wranglerrefactoring.core.WranglerRefactoring;
import org.erlide.wranglerrefactoring.core.renamefunction.NewFunctionNameInputPage;
import org.erlide.wranglerrefactoring.ui.WranglerRefactoringWizard;

public class FunExtractionRefactoringWizard extends WranglerRefactoringWizard {

	public FunExtractionRefactoringWizard(WranglerRefactoring refactoring,
			int flags) {
		super(refactoring, flags);
	}

	@Override
	protected void addUserInputPages() {
		addPage(new NewFunctionNameInputPage("Get new function name"));
	}

	@Override
	protected String initTitle() {
		return "Fun extraction refactoring";
	}

}
