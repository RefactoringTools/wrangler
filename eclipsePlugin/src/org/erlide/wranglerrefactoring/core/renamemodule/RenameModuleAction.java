package org.erlide.wranglerrefactoring.core.renamemodule;

import org.erlide.wranglerrefactoring.core.WranglerRefactoring;
import org.erlide.wranglerrefactoring.ui.WranglerRefactoringAction;
import org.erlide.wranglerrefactoring.ui.WranglerRefactoringWizard;

public class RenameModuleAction extends WranglerRefactoringAction {

	@Override
	public void dispose() {
		// TODO Auto-generated method stub

	}

	@Override
	protected String initRefactoringName() {
		return "Rename module";
	}

	@Override
	protected WranglerRefactoring initWranglerRefactoring() {
		return new RenameModuleRefactoring(parameters);
	}

	@Override
	protected WranglerRefactoringWizard initWranglerRefactoringWizard() {
		return new RenameModuleWizard(refactoring,
				WranglerRefactoringWizard.DIALOG_BASED_USER_INTERFACE);
	}

}
