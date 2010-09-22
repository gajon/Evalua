// From Douglas Crockford's book.
//if (typeof Object.beget !== 'function') {
    //Object.beget = function (o) {
        //var F = function () {};
        //F.prototype = o;
        //return new F();
    //};
//}

// http://happygiraffe.net/blog/2007/09/26/jquery-logging/
jQuery.fn.log = function (msg) {
    console.log("%s: %o", msg, this);
    return this;
};

/*
The structure of a question is this:

<div class="dummy-question type-of-question">
    <div class="question">
        <div class="input"><input type="text"/></div>
        <div class="toolbar"><a ...><a ...></div>
    </div>
    <div class="answers">
        <div class="answer-line">
            <div class="input"><input .../></div>
            <div class="toolbar">
                <a><img><span>Text</span></a>
                <a><img><span>Text</span></a>
                ...
            </div>
        </div>
        <div class="answer-line">...</div>
        ...
        ...
    </div>
    <div class="answer-add">
        <a href="#">Add another answer</a>
    </div>
</div>
*/

var Quiztronic = {
    incrementalCounter: 1,

    createForm: function (type) {
        var opts = {
            text: 'Describe una pregunta de opción múltiple',
            control: 'radio-choice',
            answers: null
        };

        if (type === 'radio-choice') {
            opts.answers = [
                {control:'radio-choice', text:'Describe una posible respuesta.'},
                {control:'radio-choice', text:'Describe otra posible respuesta.'},
                {control:'radio-choice', text:'Describe otra posible respuesta.'}
            ];

        } else if (type === 'true-false') {
            opts.text = 'Describe una pregunta de falso-verdadero';
            opts.control = 'true-false';
            opts.answers = [
                {control:'true-false', text:'Verdadero'},
                {control:'true-false', text:'Falso'}
            ];

        } else if (type === 'checkboxes') {
            opts.text = 'Describe una pregunta de selección múltiple';
            opts.control = 'checkbox';
            opts.answers = [
                {control:'checkbox', text:'Describe una de las posibles respuestas.'},
                {control:'checkbox', text:'Describe otra de las posibles respuestas.'},
                {control:'checkbox', text:'Describe otra de las posibles respuestas.'}
            ];

        } else if (type === 'textarea') {
            opts.text = 'Describe una pregunta de texto libre';
            opts.control = 'textarea';
            opts.answers = [
                {control:'textarea', text:'Texto libre...'}
            ];
        }

        return this.createQuestion(opts);
    },

    // TODO: Not used.
    updateAddArea: function (droparea) {
        // TODO: could optimize this by hiding/showing a single object.
        if ($(droparea).children().length === 0) {
            $(droparea).html('<h2>Arrastra tus preguntas aquí</h2>');
        } else {
            $(droparea).find('h2').remove();
        }
    },

    // ANSWERS AND WRAPPERS
    makeAnswerInput: function (options) {
        // Set defaults for options.
        var opts = options || {},
            classname = opts.classname || 'radio-choice',
            value = opts.value || 'Describe a possible answer.',
            toolbar = (opts.toolbar === undefined) ? true : opts.toolbar,
            remove = (classname !== 'true-false') ? true : false,
            group = opts.group || 0,
            selected = opts.selected || false;

        var selectCtrl = null,
            answer = null,
            answerLine = $('<div class="answer-line"></div>'),
            answerWrapper = $('<div class="input"></div>'),
            selectionWrapper = $('<div class="selection"></div>');

        if (classname === 'textarea') {
            answer = $('<textarea></textarea>').val(value).addClass(classname).attr('readonly', 'readonly');
            toolbar = false;
        } else {
            answer = $('<input type="text"/>').val(value).addClass(classname);
            answerLine.addClass('idle');
        }

        if (classname === 'radio-choice' || classname === 'true-false') {
            selectCtrl = $('<input type="radio" value="yes" />').attr('name', 'group'+group);
            if (selected) {
                selectCtrl.attr('checked', 'checked');
            }
            selectionWrapper.append(selectCtrl);
            answerLine.append(selectionWrapper);

        } else if (classname === 'checkbox') {
            selectCtrl = $('<input type="checkbox" value="yes" />');
            if (selected) {
                selectCtrl.attr('checked', 'checked');
            }
            selectionWrapper.append(selectCtrl);
            answerLine.append(selectionWrapper);
        }

        answerWrapper.append(answer);
        answerLine.append(answerWrapper);
        if (toolbar) {
            answerLine.append(this.makeAnswerToolbar(answerLine, remove));
        }

        if (classname !== 'textarea') {
            $(answerLine).hover(function() {
                if ($(this).hasClass('idle')) {
                    $(this).addClass('highlight');
                    $(this).removeClass('idle');
                }
            }, function() {
                if ($(this).hasClass('highlight')) {
                    $(this).removeClass('highlight');
                    $(this).addClass('idle');
                }
            });

            $(answer).focus(function () {
                $(this).
                    parent().
                    parent().
                    removeClass('idle').
                    removeClass('highlight');
                this.select();
            });

            $(answer).blur(function () {
                $(this).parent().parent().addClass('idle');
            });
        }

        return answerLine;
    },

    answerAddHelper: function (classname, value, target, group) {
        var that = this, icon, addLink;

        if (classname === 'radio-choice' || classname === 'checkbox') {
            icon = $('<img src="/static/icons/add.png" height="16" width="16" />');
            addLink = $('<a href="#"></a>');

            addLink.append(icon).append('Agregar otra respuesta');

            $(addLink).click(function (e) {
                e.preventDefault();
                $(target).append(that.makeAnswerInput({
                    classname: classname,
                    value: value,
                    group: group
                }));
            });

            return $('<div class="answer-add"></div>').append(addLink);
        }

        return $('<div class="answer-add">&nbsp;</div>');
    },

    makeAnswersContainer: function (container) {
        var div = $('<div class="answers"></div>');
        container.append(div);

        return div;
    },

    makeAnswerToolbar: function (container, removep) {
        var div = $('<div class="toolbar"></div>');
        div.append(this.makeUpLink(container));
        div.append(this.makeDownLink(container));

        if (removep) {
            div.append(this.makeRemoveLink(container));
        }

        return div;
    },

    // QUESTION INPUT AND WRAPPERS
    makeQuestionInput: function (container, text, classname) {
        var questionDiv = $('<div class="question idle"></div>');
        var inputDiv = $('<div class="input"></div>');
        var input = $('<input type="text" />').val(text).addClass(classname);
        var toolbar = this.makeQuestionToolbar(container);

        container.append(questionDiv);
        questionDiv.append(inputDiv);
        questionDiv.append(toolbar);
        inputDiv.append(input);

		$(questionDiv).hover(function() {
            if ($(this).hasClass('idle')) {
                $(this).addClass('highlight');
                $(this).removeClass('idle');
            }
		}, function() {
            if ($(this).hasClass('highlight')) {
                $(this).removeClass('highlight');
                $(this).addClass('idle');
            }
		});

        $(input).focus(function () {
            $(this).
                parent().
                parent().
                removeClass('idle').
                removeClass('highlight');
            this.select();
        });

        $(input).blur(function () {
            $(this).parent().parent().addClass('idle');
        });

        return questionDiv;
    },

    makeQuestionToolbar: function (container) {
        var div = $('<div class="toolbar"></div>');
        div.append(this.makeUpLink(container, 'Subir'));
        div.append(this.makeDownLink(container, 'Bajar'));
        div.append(this.makeRemoveLink(container, 'Quitar'));

        return div;
    },

    makeUpLink: function (container, label) {
        var link = $('<a href="#"></a>'), self = this;
        var icon = $('<img src="/static/icons/arrow_up.png" height="16" width="16" />');

        label = typeof label === 'string' ? label : "";

        if (container) {
            $(link).append(icon);
            if (label) {
                $(link).append($('<span></span>').append(label));
            }
            $(link).click(function (e) {
                var prev = $(container).prev();
                e.preventDefault();

                if (prev.length > 0) {
                    $(container).hide();
                    $(container).detach();
                    $(prev).before(container);
                    $(container).fadeIn(400);
                }
            });
            return link;
        }
        return null;
    },

    makeDownLink: function (container, label) {
        var link = $('<a href="#"></a>'), self = this;
        var icon = $('<img src="/static/icons/arrow_down.png" height="16" width="16" />');

        label = typeof label === 'string' ? label : "";

        if (container) {
            $(link).append(icon);
            if (label) {
                $(link).append($('<span></span>').append(label));
            }
            $(link).click(function (e) {
                var next = $(container).next();
                e.preventDefault();

                if (next.length > 0) {
                    $(container).hide();
                    $(container).detach();
                    $(next).after(container);
                    $(container).fadeIn(400);
                }
            });
            return link;
        }
        return null;
    },

    makeRemoveLink: function (container, label) {
        var removeLink = $('<a href="#"></a>'), self = this;
        var icon = $('<img src="/static/icons/delete.png" height="16" width="16" />');
        //label = typeof label === 'string' ? label : "Quitar";

        if (container) {
            $(removeLink).append(icon);
            if (label) {
                $(removeLink).append($('<span></span>').append(label));
            }
            $(removeLink).click(function (e) {
                e.preventDefault();
                $(container).fadeOut();
                //var droparea = $(container).parent();
                //self.updateAddArea(droparea);
            });
            return removeLink;
        }
        return null;
    },

    // CREATE FORMS
    createQuestion: function (json) {
        var that = this;
        var questionContainer = $('<div class="dummy-question"></div>');
        var question = this.makeQuestionInput(
                            questionContainer, json.text, json.control);
        var answersContainer = this.makeAnswersContainer(questionContainer);
        var group = this.incrementalCounter++;

        $.each(json.answers, function (index, json) {
            $(answersContainer).append(
                that.makeAnswerInput({
                    classname: json.control,
                    value: json.text,
                    group: group,
                    selected: json.selected }));
        });

        $(questionContainer).append(
            this.answerAddHelper(
                json.control,
                'Describe otra posible respuesta.',
                answersContainer,
                group));

        return questionContainer;
    },

    // COLLECT FORMS
    collectQuestionForms: function (container) {
        var questions = [],
            self = this;

        $(container).find('.dummy-question').each(function () {
            questions.push(self.collectSingleQuestion(this));
        });

        return questions;
    },

    collectSingleQuestion: function (container) {
        var input = $(container).find('div.question input'); 
        var q = {
            control: input.attr('class'),
            text: input.val(),
            answers: []
        };

        $(container).find('div.answer-line').each(function () {
            var selectedC = $(this).find('.selection').find('input');
            var selected = false;

            if (selectedC.length > 0 && selectedC.attr('checked') === true) {
                selected = true;
            }

            var control = $(this).find('.input').find('input');
            if (control.length === 0) {
                control = $(this).find('textarea');
            }

            if (control.length !== 0) {
                q.answers.push({
                    control: $(control).attr('class'),
                    text: $(control).val(),
                    selected: selected
                });
            }
        });

        return q;
    }
};


$(document).ready(function () {
    var formId = $('#id_id').val();
    var addArea = $('#questions-addarea');

    $('div.buttons a').click(function () {
        var form = Quiztronic.createForm($(this).attr('id'));
        if (form) {
            $(addArea).append(form);
        }
    });


    // ==================================================
    // SUBMIT THE EDITED FORM
    // ==================================================
    $('#questions-submit #id_submit').click(function () {
        var formTitle = $('#id_title').val(),
            formNotes = $('#id_notes').val(),
            questions = Quiztronic.collectQuestionForms(addArea);

        $.post('/design/backend-save-form/',
            {
            id: formId,
            title: formTitle,
            notes: formNotes,
            questions: JSON.stringify(questions),
            },
            function (res) {
                var errorMsg,
                    position;

                if (res && res.status && res.status === 'ok' && res.id) {
                    location.pathname = '/design/edit-form-options/?id=' + res.id;

                } else if (res && res.status && res.status === 'error' && res.error) {

                    if (res.error === 'title') {
                        $('#id_title').addClass('error');

                        errorMsg = $('<div class="error-msg"></div>');
                        errorMsg.append('El título es obligatorio.');
                        $('#id_title').parent().append(errorMsg);

                        position = $('#id_title').offset().top - 50;
                        $(window).scrollTop(position);

                    } else if (res.error === 'empty-questions') {
                        errorMsg = $('<div class="error-msg"></div>');
                        errorMsg.append("La evaluación requiere al menos una pregunta.");
                        $('#questions-addarea').append(errorMsg);
                    }
                    else {
                        $('#questions-submit').append($('<h1>asdfasdf</h1>'));
                    }

                } else {
                    $('#questions-submit').append($('<h1>Error</h1>'));
                }
            },
            'json');
    });


    // ==================================================
    // LOAD THE FORM'S DATA AND QUESTIONS
    // ==================================================
    $.getJSON('/design/backend-get-form-questions',
        { id: formId },
        function (json) {
            if (json && json.length && json.length > 0) {
                $.each(json, function (index, jsonQuestion) {
                    var question = Quiztronic.createQuestion(jsonQuestion);
                    if (question) {
                        $(addArea).append(question);
                    }
                });
            }
        });
});

// vim: set sw=4 ts=4 et:
