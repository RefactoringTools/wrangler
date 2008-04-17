package org.erlide.wranglerrefactoring.core.generalise;

import org.erlide.wranglerrefactoring.core.WranglerRefactoring;
import org.erlide.wranglerrefactoring.ui.WranglerRefactoringWizard;

public class GeneraliseWizard extends WranglerRefactoringWizard {

	public GeneraliseWizard(WranglerRefactoring refactoring, int flags) {
		super(refactoring, flags);
	}

	@Override
	protected void addUserInputPages() {
		addPage(new NewParameterNameInputPage("Get new parameter name"));
	}

	@Override
	protected String initTitle() {
		return "Generalise function refactoring";
	}

}
