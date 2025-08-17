sap.ui.define([
	"sap/ui/core/mvc/Controller",
	"sap/ui/model/json/JSONModel",
	"sap/ui/core/Fragment",
	"sap/m/MessageToast",
	"sap/ui/model/Filter",
	"sap/ui/model/FilterOperator",
	"com/example/requests/util/formatter"
], function (Controller, JSONModel, Fragment, MessageToast, Filter, FilterOperator, formatter) {
	"use strict";

	return Controller.extend("com.example.requests.controller.Detail", {
		formatter: formatter,

		onInit: function () {
			this.getOwnerComponent().getRouter().getRoute("Detail").attachPatternMatched(this._onRouteMatched, this);
		},

		_onRouteMatched: function (oEvent) {
			var sPath = decodeURIComponent(oEvent.getParameter("arguments").path);
			this.getView().bindElement({ path: "/" + sPath });
		},

		onAddPosition: function () {
			var oView = this.getView();
			if (!this._pDialog) {
				this._pDialog = Fragment.load({
					name: "com.example.requests.fragment.PositionDialog",
					controller: this
				}).then(function (oDialog) {
					oView.addDependent(oDialog);
					return oDialog;
				});
			}
			this._pDialog.then(function (oDialog) {
				var oNew = { Amount: null, Currency: "UAH", Description: "", DepartmentId: "" };
				oDialog.setModel(new JSONModel(oNew), "pos");
				oDialog.open();
			}.bind(this));
		},

		onSavePosition: function () {
			var oDialog = this.byId("positionDialog");
			var oPos = oDialog.getModel("pos").getData();
			var oBinding = this.byId("positionsTable").getBinding("items");
			oBinding.create(oPos);
			oDialog.close();
			MessageToast.show(this.getResourceBundle().getText("msg.positionAdded"));
		},

		onCancelPosition: function () {
			this.byId("positionDialog").close();
		},

		onDeletePosition: function (oEvent) {
			var oCtx = oEvent.getParameter("listItem").getBindingContext();
			oCtx.delete().then(function () {
				MessageToast.show(this.getResourceBundle().getText("msg.positionDeleted"));
			}.bind(this));
		},

		onDepartmentValueHelp: function (oEvent) {
			var oInput = oEvent.getSource();
			var oView = this.getView();
			if (!this._pDeptVH) {
				this._pDeptVH = Fragment.load({
					name: "com.example.requests.fragment.DepartmentValueHelp",
					controller: this
				}).then(function (oDialog) {
					oView.addDependent(oDialog);
					return oDialog;
				});
			}
			this._pDeptVH.then(function (oDialog) {
				oDialog.data("field", oInput);
				oDialog.open();
			});
		},

		onDepartmentSearch: function (oEvent) {
			var sValue = oEvent.getParameter("value");
			var oFilter = new Filter({
				filters: [
					new Filter("DepartmentId", FilterOperator.Contains, sValue),
					new Filter("Name", FilterOperator.Contains, sValue)
				],
				and: false
			});
			oEvent.getSource().getBinding("items").filter([oFilter]);
		},

		onDepartmentConfirm: function (oEvent) {
			var oSelectedItem = oEvent.getParameter("selectedItem");
			if (!oSelectedItem) { return; }
			var oSelected = oSelectedItem.getBindingContext().getObject();
			var oField = oEvent.getSource().data("field");
			oField.setValue(oSelected.DepartmentId);
		},

		onDepartmentCancel: function () {
			// no-op
		},

		getResourceBundle: function () {
			return this.getOwnerComponent().getModel("i18n").getResourceBundle();
		}
	});
});