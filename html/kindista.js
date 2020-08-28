function KsetDisplay(Kelement, Kvalue) {
    return Kelement.style.display = Kvalue;
};
function KsubmitImageForm(Kimageform, Kspinner) {
    console.log(Kimageform);
    console.log(Kspinner);
    console.log(document.getElementById(Kspinner));
    KsetDisplay(document.getElementById(Kspinner), 'inline-block');
    return document.getElementById(Kimageform).submit();
};
function KsubmitMarkdownForm() {
    KsetDisplay(document.getElementById('spinner'), 'inline-block');
    return document.forms.markdownFile.submit();
};
function KlimitCharacters(Kmessage, KcharCount, Kindicator) {
    Kchars = Kmessage.value.length;
    console.log(document.getElementById(Kindicator).innerHTML);
    console.log(document.getElementById(Kindicator));
    document.getElementById(Kindicator).innerHTML = KcharCount - Kchars;
    return Kchars > KcharCount ? (Kmessage.value = Kmessage.value.substring(0, KcharCount)) : null;
};
function acceptCookies() {
    document.getElementById('cookiedisclaimer').style.visibility = 'hidden';
};
