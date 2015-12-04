function KsetDisplay(Kelement, Kvalue) {
    return Kelement.style.display = Kvalue;
};
var KsubmitImageForm = function (Kparam) {
    console.log(Kparam);
    console.log(Kparam.form);
    KsetDisplay(Kparam.form.getElementsByClassName('spinner'), 'inline-block');
    return Kparam.form.submit();
};
function KsubmitImageForm2(Kparam) {
    console.log(Kparam);
    console.log(Kparam.form);
    KsetDisplay(Kparam.form.getElementsByClassName('spinner'), 'inline-block');
    return Kparam.form.submit();
};
function KsubmitImageFormOld(Kimageform, Kspinner) {
    console.log(Kspinner);
    console.log(document.getElementById(Kspinner));
    KsetDisplay(document.getElementById(Kspinner), 'inline-block');
    return document.forms.Kimageform.submit();
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
