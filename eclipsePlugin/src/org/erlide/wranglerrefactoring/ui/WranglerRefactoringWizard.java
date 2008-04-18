package org.erlide.wranglerrefactoring.ui;

import org.eclipse.ltk.ui.refactoring.RefactoringWizard;
import org.erlide.wranglerrefactoring.core.WranglerRefactoring;

public abstract class WranglerRefactoringWizard extends RefactoringWizard {

	public WranglerRefactoringWizard(WranglerRefactoring refactoring, int flags) {
		super(refactoring, flags);
		setWindowTitle(initTitle());
		// setForcePreviousAndNextButtons(true);
		setHelpAvailable(false);
		setDefaultPageTitle(initTitle());
	}

	protected abstract String initTitle();

	@Override
	/**
	 * Implementors should add pages, and set the title..
	 */
	protected abstract void addUserInputPages();

}
