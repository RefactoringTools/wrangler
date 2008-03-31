package org.erlide.wranglerrefactoring.core.renamevariable;

import org.erlide.wranglerrefactoring.core.WranglerRefactoring;
import org.erlide.wranglerrefactoring.ui.WranglerRefactoringWizard;

public class RenameVariableWizard extends WranglerRefactoringWizard {

	public RenameVariableWizard(WranglerRefactoring refactoring, int flags) {
		super(refactoring, flags);
	}

	@Override
	protected void addUserInputPages() {
		addPage(new NewVariableNameInputPage("Get new variable name"));
	}

	@Override
	protected String initTitle() {
		return "Rename variable refactoring";
	}

}
