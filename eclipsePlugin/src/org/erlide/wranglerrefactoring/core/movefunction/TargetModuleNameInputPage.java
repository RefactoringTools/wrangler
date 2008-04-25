package org.erlide.wranglerrefactoring.core.movefunction;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.erlide.wranglerrefactoring.core.renamemodule.NewModuleNameInputPage;

public class TargetModuleNameInputPage extends NewModuleNameInputPage {

	private Button checkIsNewModule;

	public TargetModuleNameInputPage(String name) {
		super(name);
	}

	@Override
	protected String initDescription() {
		return "Move function to another module";
	}

	@Override
	protected void initExtraControls(GridLayout layout) {
		checkIsNewModule = new Button(composite, SWT.CHECK);
		checkIsNewModule.setText("new module");
		GridData gridData = new GridData();
		gridData.horizontalAlignment = GridData.FILL;
		gridData.horizontalSpan = 2;
		checkIsNewModule.setLayoutData(gridData);

		checkIsNewModule.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(org.eclipse.swt.events.SelectionEvent e) {
				((MoveFunctionRefactoring) getRefactoring())
						.setIsNewModule(checkIsNewModule.getSelection());
			}
		});

	}

	@Override
	protected String initLabelText() {
		return "Target module name:";
	}

	@Override
	protected String initTitle() {
		return "Move function";
	}

}
