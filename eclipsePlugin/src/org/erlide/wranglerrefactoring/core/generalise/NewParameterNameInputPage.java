package org.erlide.wranglerrefactoring.core.generalise;

import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.MessageBox;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.wranglerrefactoring.core.exception.WranglerException;
import org.erlide.wranglerrefactoring.ui.WranglerNewDataInputPage;

public class NewParameterNameInputPage extends WranglerNewDataInputPage {

	public NewParameterNameInputPage(String name) {
		super(name);
	}

	@Override
	protected String initDescription() {
		return "Generalise the selected function";
	}

	@Override
	protected String initLabelText() {
		return "New parameter name:";
	}

	@Override
	protected void initListeners() {
		newDataText.addModifyListener(new ModifyListener() {

			@Override
			public void modifyText(ModifyEvent e) {
				// TODO: not exact, whitespaces??
				String s = newDataText.getText();
				if (s.length() == 0) {
					setPageComplete(false);
					setErrorMessage(null);
				} else if (!s.substring(0, 1).toUpperCase().equals(
						s.substring(0, 1))) {
					setPageComplete(false);
					setErrorMessage("Variable name must start with an uppercase letter!");
				} else {
					setPageComplete(true);
				}

			}

		});
	}

	@Override
	protected String initTitle() {
		return "Genralise function";
	}

	private void callWranglerRPCs() {
		GeneraliseRefactoring refac = (GeneraliseRefactoring) getRefactoring();

		try {
			GeneraliseRPCMessage m = refac.callGenerealise();

			if (m.getGeneraliseStatus() == GeneraliseRPCMessage.FREEVARS) {
				boolean response = showYesOrNoMessageBox(
						"The selected expression has free variables, do you want to continue the refactoring?",
						"Free variables!");
				if (response)
					m = refac.callGeneralise2();
				else {
					// TODO: EXIT!!!???
				}

			}

			if (m.getGeneraliseStatus() == GeneraliseRPCMessage.UNKNOWNSIDEEFFECT) {
				boolean response = showYesOrNoMessageBox(
						"Does the selected expression have side effects?",
						"Side effect");
				refac.setSideEffect(response);
				refac.callGeneralise1();
			}
		} catch (ErlangRpcException e) {
			refac.addRefactoringStatus(RefactoringStatus
					.createFatalErrorStatus(e.getMessage()));
		} catch (RpcException e) {
			refac.addRefactoringStatus(RefactoringStatus
					.createFatalErrorStatus(e.getMessage()));
		} catch (WranglerException e) {
			refac.addRefactoringStatus(RefactoringStatus
					.createFatalErrorStatus(e.getMessage()));
		}
	}

	private boolean showYesOrNoMessageBox(String question, String title) {
		boolean b;
		MessageBox mb = new MessageBox(this.getShell(), SWT.ICON_WARNING
				| SWT.YES | SWT.NO);
		mb.setMessage(question);
		mb.setText(title);
		int response = mb.open();
		if (response == SWT.YES)
			b = true;
		else
			b = false;

		return b;
	}

	@Override
	public boolean performFinish() {
		callWranglerRPCs();
		return super.performFinish();
	}

	@Override
	public IWizardPage getNextPage() {
		// it is not the best solution
		callWranglerRPCs();
		return super.getNextPage();
	}
}
