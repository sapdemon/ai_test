sap.ui.define([
	"sap/ui/core/UIComponent",
	"com/example/requests/model/models"
], function (UIComponent, models) {
	"use strict";

	return UIComponent.extend("com.example.requests.Component", {
		metadata: {
			manifest: "json"
		},

		init: function () {
			UIComponent.prototype.init.apply(this, arguments);
			this.setModel(models.createDeviceModel(), "device");
			this.getRouter().initialize();
		}
	});
});