package org.erlide.wranglerrefactoring;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.runtime.backend.internal.ManagedBackend;
import org.osgi.framework.BundleContext;

import erlang.ErlangCode;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "org.erlide.wranglerrefactoring";

	// The shared instance
	private static Activator plugin;

	/**
	 * The constructor
	 */
	public Activator() {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);

		initWrangler();
		plugin = this;
	}

	// TODO: read from settings file
	@SuppressWarnings("restriction")
	private void initWrangler() {
		String wranglerPath = "/home/mee/Programok/wrangler/ebin";
		ManagedBackend mb = (ManagedBackend) BackendManager.getDefault()
				.getIdeBackend();
		ErlangCode.addPathA(mb, wranglerPath);

		try {
			mb.rpc("code", "load_file", "a", "wrangler");
		} catch (ErlangRpcException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (RpcException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance
	 * 
	 * @return the shared instance
	 */
	public static Activator getDefault() {
		return plugin;
	}

}
