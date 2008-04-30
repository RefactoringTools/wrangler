package org.erlide.wranglerrefactoring.ui;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ltk.ui.refactoring.RefactoringWizardOpenOperation;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.erlide.wranglerrefactoring.core.RefactoringParameters;
import org.erlide.wranglerrefactoring.core.WranglerRefactoring;

//TODO: check the init, dispose,selecionChanged methods, it could be useful - cdtrefac
public abstract class WranglerRefactoringAction implements
		IWorkbenchWindowActionDelegate, IEditorActionDelegate {

	protected RefactoringParameters parameters = new RefactoringParameters();

	protected WranglerRefactoring refactoring;

	protected WranglerRefactoringWizard refactoringWizard;

	protected String refactoringName;

	@Override
	/**
	 * implementors should set the refactoring name here
	 */
	public abstract void dispose();

	@Override
	public void init(IWorkbenchWindow window) {

	}

	@Override
	public void run(IAction action) {

		parameters.initSelection();

		refactoringName = initRefactoringName();
		refactoring = initWranglerRefactoring();
		refactoringWizard = initWranglerRefactoringWizard();

		RefactoringWizardOpenOperation op = new RefactoringWizardOpenOperation(
				refactoringWizard);

		Shell shell = new Shell();

		try {
			op.run(shell, refactoringName);
		} catch (InterruptedException e) {
			// TODO what the hell is needed here?
			e.printStackTrace();
		}

	}

	protected abstract String initRefactoringName();

	protected abstract WranglerRefactoringWizard initWranglerRefactoringWizard();

	protected abstract WranglerRefactoring initWranglerRefactoring();

	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		parameters.setSelection(selection);
	}

	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
		parameters.setEditorPart(targetEditor);
	}

}
