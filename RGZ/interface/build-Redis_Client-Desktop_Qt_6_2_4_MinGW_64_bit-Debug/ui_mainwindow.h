/********************************************************************************
** Form generated from reading UI file 'mainwindow.ui'
**
** Created by: Qt User Interface Compiler version 6.2.4
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_MAINWINDOW_H
#define UI_MAINWINDOW_H

#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QGroupBox>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QMenuBar>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QSpacerItem>
#include <QtWidgets/QStatusBar>
#include <QtWidgets/QTextEdit>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_MainWindow
{
public:
    QWidget *centralwidget;
    QTextEdit *textEdit;
    QGroupBox *groupBox;
    QWidget *verticalLayoutWidget;
    QVBoxLayout *verticalLayout;
    QLabel *label;
    QLineEdit *get_pairs;
    QPushButton *get_pairs_btn;
    QSpacerItem *verticalSpacer_3;
    QLabel *label_3;
    QLineEdit *set_pairs;
    QPushButton *set_pairs_btn;
    QGroupBox *groupBox_2;
    QWidget *verticalLayoutWidget_2;
    QVBoxLayout *verticalLayout_2;
    QLabel *label_2;
    QLineEdit *hash_key;
    QPushButton *get_hash_btn;
    QSpacerItem *verticalSpacer;
    QLabel *label_6;
    QLineEdit *set_hash;
    QPushButton *set_hash_btn;
    QGroupBox *groupBox_3;
    QWidget *verticalLayoutWidget_3;
    QVBoxLayout *verticalLayout_3;
    QLabel *label_7;
    QLineEdit *list_key;
    QPushButton *get_list;
    QSpacerItem *verticalSpacer_4;
    QLabel *label_10;
    QLineEdit *list_val;
    QPushButton *list_addl;
    QPushButton *list_addr;
    QSpacerItem *verticalSpacer_6;
    QPushButton *list_delr;
    QPushButton *list_dell;
    QLabel *label_4;
    QMenuBar *menubar;
    QStatusBar *statusbar;

    void setupUi(QMainWindow *MainWindow)
    {
        if (MainWindow->objectName().isEmpty())
            MainWindow->setObjectName(QString::fromUtf8("MainWindow"));
        MainWindow->resize(1268, 459);
        centralwidget = new QWidget(MainWindow);
        centralwidget->setObjectName(QString::fromUtf8("centralwidget"));
        textEdit = new QTextEdit(centralwidget);
        textEdit->setObjectName(QString::fromUtf8("textEdit"));
        textEdit->setGeometry(QRect(0, 50, 401, 351));
        textEdit->setReadOnly(true);
        groupBox = new QGroupBox(centralwidget);
        groupBox->setObjectName(QString::fromUtf8("groupBox"));
        groupBox->setGeometry(QRect(410, 10, 281, 281));
        verticalLayoutWidget = new QWidget(groupBox);
        verticalLayoutWidget->setObjectName(QString::fromUtf8("verticalLayoutWidget"));
        verticalLayoutWidget->setGeometry(QRect(0, 20, 281, 261));
        verticalLayout = new QVBoxLayout(verticalLayoutWidget);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        verticalLayout->setContentsMargins(0, 0, 0, 0);
        label = new QLabel(verticalLayoutWidget);
        label->setObjectName(QString::fromUtf8("label"));

        verticalLayout->addWidget(label);

        get_pairs = new QLineEdit(verticalLayoutWidget);
        get_pairs->setObjectName(QString::fromUtf8("get_pairs"));

        verticalLayout->addWidget(get_pairs);

        get_pairs_btn = new QPushButton(verticalLayoutWidget);
        get_pairs_btn->setObjectName(QString::fromUtf8("get_pairs_btn"));

        verticalLayout->addWidget(get_pairs_btn);

        verticalSpacer_3 = new QSpacerItem(20, 40, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout->addItem(verticalSpacer_3);

        label_3 = new QLabel(verticalLayoutWidget);
        label_3->setObjectName(QString::fromUtf8("label_3"));

        verticalLayout->addWidget(label_3);

        set_pairs = new QLineEdit(verticalLayoutWidget);
        set_pairs->setObjectName(QString::fromUtf8("set_pairs"));

        verticalLayout->addWidget(set_pairs);

        set_pairs_btn = new QPushButton(verticalLayoutWidget);
        set_pairs_btn->setObjectName(QString::fromUtf8("set_pairs_btn"));

        verticalLayout->addWidget(set_pairs_btn);

        groupBox_2 = new QGroupBox(centralwidget);
        groupBox_2->setObjectName(QString::fromUtf8("groupBox_2"));
        groupBox_2->setGeometry(QRect(720, 10, 231, 281));
        verticalLayoutWidget_2 = new QWidget(groupBox_2);
        verticalLayoutWidget_2->setObjectName(QString::fromUtf8("verticalLayoutWidget_2"));
        verticalLayoutWidget_2->setGeometry(QRect(0, 20, 231, 261));
        verticalLayout_2 = new QVBoxLayout(verticalLayoutWidget_2);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        verticalLayout_2->setContentsMargins(0, 0, 0, 0);
        label_2 = new QLabel(verticalLayoutWidget_2);
        label_2->setObjectName(QString::fromUtf8("label_2"));

        verticalLayout_2->addWidget(label_2);

        hash_key = new QLineEdit(verticalLayoutWidget_2);
        hash_key->setObjectName(QString::fromUtf8("hash_key"));

        verticalLayout_2->addWidget(hash_key);

        get_hash_btn = new QPushButton(verticalLayoutWidget_2);
        get_hash_btn->setObjectName(QString::fromUtf8("get_hash_btn"));

        verticalLayout_2->addWidget(get_hash_btn);

        verticalSpacer = new QSpacerItem(20, 40, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_2->addItem(verticalSpacer);

        label_6 = new QLabel(verticalLayoutWidget_2);
        label_6->setObjectName(QString::fromUtf8("label_6"));

        verticalLayout_2->addWidget(label_6);

        set_hash = new QLineEdit(verticalLayoutWidget_2);
        set_hash->setObjectName(QString::fromUtf8("set_hash"));

        verticalLayout_2->addWidget(set_hash);

        set_hash_btn = new QPushButton(verticalLayoutWidget_2);
        set_hash_btn->setObjectName(QString::fromUtf8("set_hash_btn"));

        verticalLayout_2->addWidget(set_hash_btn);

        groupBox_3 = new QGroupBox(centralwidget);
        groupBox_3->setObjectName(QString::fromUtf8("groupBox_3"));
        groupBox_3->setGeometry(QRect(970, 10, 281, 401));
        verticalLayoutWidget_3 = new QWidget(groupBox_3);
        verticalLayoutWidget_3->setObjectName(QString::fromUtf8("verticalLayoutWidget_3"));
        verticalLayoutWidget_3->setGeometry(QRect(0, 20, 281, 381));
        verticalLayout_3 = new QVBoxLayout(verticalLayoutWidget_3);
        verticalLayout_3->setObjectName(QString::fromUtf8("verticalLayout_3"));
        verticalLayout_3->setContentsMargins(0, 0, 0, 0);
        label_7 = new QLabel(verticalLayoutWidget_3);
        label_7->setObjectName(QString::fromUtf8("label_7"));

        verticalLayout_3->addWidget(label_7);

        list_key = new QLineEdit(verticalLayoutWidget_3);
        list_key->setObjectName(QString::fromUtf8("list_key"));

        verticalLayout_3->addWidget(list_key);

        get_list = new QPushButton(verticalLayoutWidget_3);
        get_list->setObjectName(QString::fromUtf8("get_list"));

        verticalLayout_3->addWidget(get_list);

        verticalSpacer_4 = new QSpacerItem(20, 40, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_3->addItem(verticalSpacer_4);

        label_10 = new QLabel(verticalLayoutWidget_3);
        label_10->setObjectName(QString::fromUtf8("label_10"));

        verticalLayout_3->addWidget(label_10);

        list_val = new QLineEdit(verticalLayoutWidget_3);
        list_val->setObjectName(QString::fromUtf8("list_val"));

        verticalLayout_3->addWidget(list_val);

        list_addl = new QPushButton(verticalLayoutWidget_3);
        list_addl->setObjectName(QString::fromUtf8("list_addl"));

        verticalLayout_3->addWidget(list_addl);

        list_addr = new QPushButton(verticalLayoutWidget_3);
        list_addr->setObjectName(QString::fromUtf8("list_addr"));

        verticalLayout_3->addWidget(list_addr);

        verticalSpacer_6 = new QSpacerItem(20, 24, QSizePolicy::Minimum, QSizePolicy::Fixed);

        verticalLayout_3->addItem(verticalSpacer_6);

        list_delr = new QPushButton(verticalLayoutWidget_3);
        list_delr->setObjectName(QString::fromUtf8("list_delr"));

        verticalLayout_3->addWidget(list_delr);

        list_dell = new QPushButton(verticalLayoutWidget_3);
        list_dell->setObjectName(QString::fromUtf8("list_dell"));

        verticalLayout_3->addWidget(list_dell);

        label_4 = new QLabel(centralwidget);
        label_4->setObjectName(QString::fromUtf8("label_4"));
        label_4->setGeometry(QRect(130, 30, 121, 16));
        MainWindow->setCentralWidget(centralwidget);
        menubar = new QMenuBar(MainWindow);
        menubar->setObjectName(QString::fromUtf8("menubar"));
        menubar->setGeometry(QRect(0, 0, 1268, 21));
        MainWindow->setMenuBar(menubar);
        statusbar = new QStatusBar(MainWindow);
        statusbar->setObjectName(QString::fromUtf8("statusbar"));
        MainWindow->setStatusBar(statusbar);

        retranslateUi(MainWindow);

        QMetaObject::connectSlotsByName(MainWindow);
    } // setupUi

    void retranslateUi(QMainWindow *MainWindow)
    {
        MainWindow->setWindowTitle(QCoreApplication::translate("MainWindow", "MainWindow", nullptr));
        groupBox->setTitle(QCoreApplication::translate("MainWindow", "\320\240\320\260\320\261\320\276\321\202\320\260 \321\201 \320\277\320\260\321\200\320\260\320\274\320\270 \320\272\320\273\321\216\321\207-\320\267\320\275\320\260\321\207\320\265\320\275\320\270\320\265", nullptr));
        label->setText(QCoreApplication::translate("MainWindow", "\320\243\320\267\320\275\320\260\321\202\321\214 \320\267\320\275\320\260\321\207\320\265\320\275\320\270\321\217 \320\272\320\273\321\216\321\207\320\265\320\271 key1, key2, ...", nullptr));
        get_pairs_btn->setText(QCoreApplication::translate("MainWindow", "\320\222\321\213\320\277\320\276\320\273\320\275\320\270\321\202\321\214", nullptr));
        label_3->setText(QCoreApplication::translate("MainWindow", "\320\227\320\260\320\264\320\260\321\202\321\214 \320\267\320\275\320\260\321\207\320\265\320\275\320\270\321\217 \320\272\320\273\321\216\321\207\320\265\320\271 key1=val1, key2=val2, ...", nullptr));
        set_pairs_btn->setText(QCoreApplication::translate("MainWindow", "\320\222\321\213\320\277\320\276\320\273\320\275\320\270\321\202\321\214", nullptr));
        groupBox_2->setTitle(QCoreApplication::translate("MainWindow", "\320\240\320\260\320\261\320\276\321\202\320\260 \321\201 \321\205\321\215\321\210\320\260\320\274\320\270", nullptr));
        label_2->setText(QCoreApplication::translate("MainWindow", "\320\232\320\273\321\216\321\207 key", nullptr));
        get_hash_btn->setText(QCoreApplication::translate("MainWindow", "\320\243\320\267\320\275\320\260\321\202\321\214 \320\267\320\275\320\260\321\207\320\265\320\275\320\270\320\265 \321\205\321\215\321\210\320\260 \320\277\320\276 \320\272\320\273\321\216\321\207\321\203 key", nullptr));
        label_6->setText(QCoreApplication::translate("MainWindow", "\320\227\320\275\320\260\321\207\320\265\320\275\320\270\321\217 key1=val1, key2=val2, key3=val3", nullptr));
        set_hash_btn->setText(QCoreApplication::translate("MainWindow", "\320\227\320\260\320\264\320\260\321\202\321\214 \320\267\320\275\320\260\321\207\320\265\320\275\320\270\321\217 \321\205\321\215\321\210\320\260 \320\277\320\276 \320\272\320\273\321\216\321\207\321\203 key", nullptr));
        groupBox_3->setTitle(QCoreApplication::translate("MainWindow", "\320\240\320\260\320\261\320\276\321\202\320\260 \321\201\320\276 \321\201\320\277\320\270\321\201\320\272\320\260\320\274\320\270", nullptr));
        label_7->setText(QCoreApplication::translate("MainWindow", "\320\232\320\273\321\216\321\207 key", nullptr));
        get_list->setText(QCoreApplication::translate("MainWindow", "\320\237\321\200\320\276\321\201\320\274\320\276\321\202\321\200\320\265\321\202\321\214 \321\201\320\277\320\270\321\201\320\276\320\272 key", nullptr));
        label_10->setText(QCoreApplication::translate("MainWindow", "\320\227\320\275\320\260\321\207\320\265\320\275\320\270\320\265", nullptr));
        list_addl->setText(QCoreApplication::translate("MainWindow", "\320\224\320\276\320\261\320\260\320\262\320\270\321\202\321\214 \320\267\320\275\320\260\321\207\320\265\320\275\320\270\320\265 \321\201 \320\275\320\260\321\207\320\260\320\273\320\260 \321\201\320\277\320\270\321\201\320\272\320\260", nullptr));
        list_addr->setText(QCoreApplication::translate("MainWindow", "\320\224\320\276\320\261\320\260\320\262\320\270\321\202\321\214 \320\267\320\275\320\260\321\207\320\265\320\275\320\270\320\265 \321\201 \320\272\320\276\320\275\321\206\320\260 \321\201\320\277\320\270\321\201\320\272\320\260", nullptr));
        list_delr->setText(QCoreApplication::translate("MainWindow", "\320\243\320\264\320\260\320\273\320\270\321\202\321\214 \321\215\320\273\320\265\320\274\320\265\320\275\321\202 \321\201 \320\272\320\276\320\275\321\206\320\260", nullptr));
        list_dell->setText(QCoreApplication::translate("MainWindow", "\320\243\320\264\320\260\320\273\320\270\321\202\321\214 \321\215\320\273\320\265\320\274\320\265\320\275\321\202 \321\201 \320\275\320\260\321\207\320\260\320\273\320\260", nullptr));
        label_4->setText(QCoreApplication::translate("MainWindow", "\320\240\320\265\320\267\321\203\320\273\321\214\321\202\320\260\321\202\321\213 \320\267\320\260\320\277\321\200\320\276\321\201\320\276\320\262", nullptr));
    } // retranslateUi

};

namespace Ui {
    class MainWindow: public Ui_MainWindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_MAINWINDOW_H
