package org.erlide.wranglerrefactoring.core.renamefunction;

import org.erlide.wranglerrefactoring.core.WranglerRefactoring;
import org.erlide.wranglerrefactoring.ui.WranglerRefactoringWizard;

public class RenameFunctionWizard extends WranglerRefactoringWizard {

	public RenameFunctionWizard(WranglerRefactoring refactoring, int flags) {
		super(refactoring, flags);
	}

	@Override
	protected void addUserInputPages() {
		addPage(new NewFunctionNameInputPage("Get new function name"));
	}

	@Override
	protected String initTitle() {
		return "Rename function refactoring";
	}

}
