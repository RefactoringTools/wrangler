package org.erlide.wranglerrefactoring.core.generalise;

import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.erlide.wranglerrefactoring.ui.WranglerNewNameInputPage;

public class NewParameterNameInputPage extends WranglerNewNameInputPage {

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
		newNameText.addModifyListener(new ModifyListener() {

			@Override
			public void modifyText(ModifyEvent e) {
				// TODO: not exact, whitespaces??
				String s = newNameText.getText();
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

}
