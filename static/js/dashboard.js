$(document).ready(function () {
	var tableFound = $(this).find("#id-table-active").find("tbody").find("tr");
	if (tableFound && tableFound.length != 0) {
		$("#id-table-active").tablesorter({sortList: [[0,0]]});
	}
});
