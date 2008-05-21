package org.erlide.wrangler.refactoring.core.renamefunction;

import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.erlide.wrangler.refactoring.ui.WranglerNewDataInputPage;

public class NewFunctionNameInputPage extends WranglerNewDataInputPage {

	public NewFunctionNameInputPage(String name) {
		super(name);
	}

	@Override
	protected String initDescription() {
		return "Rename the selected function";
	}

	@Override
	protected String initLabelText() {
		return "New function name:";
	}

	@Override
	protected void initListeners() {
		newDataText.addModifyListener(new ModifyListener() {

			@Override
			public void modifyText(ModifyEvent e) {
				String s = newDataText.getText();
				if (s.length() == 0) {
					setPageComplete(false);
					setErrorMessage(null);
				} else if (!s.substring(0, 1).toLowerCase().equals(
						s.substring(0, 1))) {
					// TODO: use the OtpErlangAtom class
					setPageComplete(false);
					setErrorMessage("Function name must be an atom!");
				} else {
					setPageComplete(true);
					setErrorMessage(null);
				}
			}

		});
	}

	@Override
	protected String initTitle() {
		return "Rename function name";
	}

}
