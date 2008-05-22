package org.erlide.wrangler.refactoring.core.foldexpression;

import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringWizard;

public class FoldExpressionRefactoringWizard extends WranglerRefactoringWizard {

	public FoldExpressionRefactoringWizard(WranglerRefactoring refactoring,
			int flags) {
		super(refactoring, flags);		
	}

	@Override
	protected void addUserInputPages() {
		// TODO Auto-generated method stub

	}

	@Override
	protected String initTitle() {
		return "Fold expression refactoring";
	}

}
