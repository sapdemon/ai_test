sap.ui.define([
	"sap/ui/core/format/DateFormat"
], function (DateFormat) {
	"use strict";

	var oDateFormat = DateFormat.getDateInstance({ style: "medium" });

	return {
		formatDate: function (vDate) {
			if (!vDate) { return ""; }
			var oDate = vDate instanceof Date ? vDate : new Date(vDate);
			if (isNaN(oDate.getTime())) { return ""; }
			return oDateFormat.format(oDate);
		}
	};
});