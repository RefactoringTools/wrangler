package org.erlide.wranglerrefactoring.core.movefunction;

import org.erlide.wranglerrefactoring.core.WranglerRefactoring;
import org.erlide.wranglerrefactoring.ui.WranglerRefactoringWizard;

public class Movefunctionwizard extends WranglerRefactoringWizard {

	public Movefunctionwizard(WranglerRefactoring refactoring, int flags) {
		super(refactoring, flags);
	}

	@Override
	protected void addUserInputPages() {
		addPage(new TargetModuleNameInputPage("Get target module name"));
	}

	@Override
	protected String initTitle() {
		return "Move function to another module";
	}

}
