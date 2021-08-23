void replaceAll(const wxString& from, const wxString& to, wxString& source) {
	wxString newString;
	newString.reserve(source.length());  //avoids a few memory allocations

	wxString::size_type lastPos = 0;
	wxString::size_type findPos;

	while(wxString::npos != (findPos = source.find(from, lastPos)))
	{
		newString.append(source, lastPos, findPos - lastPos);
		newString += to;
		lastPos = findPos + from.length();
	}

	newString += source.substr(lastPos);

	source.swap(newString);
}

wxString replaced(const wxString& from, const wxString& to, wxString& source) {
	wxString newString;
	newString.reserve(source.length());

	wxString::size_type lastPos = 0;
	wxString::size_type findPos;

	while(wxString::npos != (findPos = source.find(from, lastPos)))
	{
		newString.append(source, lastPos, findPos - lastPos);
		newString += to;
		lastPos = findPos + from.length();
	}

	newString += source.substr(lastPos);

	return newString;
}
