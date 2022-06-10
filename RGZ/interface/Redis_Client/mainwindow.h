#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QtNetwork/QNetworkAccessManager>
#include <QtNetwork/QNetworkRequest>
#include <QtNetwork/QNetworkReply>
#include <QUrl>

QT_BEGIN_NAMESPACE
namespace Ui { class MainWindow; }
QT_END_NAMESPACE

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    enum class Type {
        POST,
        GET,
        DEL
    };

    MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

private slots:
    void on_pushButton_clicked();
    void replyFinished(); // cлот, выполняемый при завершении запроса

    void on_get_pairs_btn_clicked();

    void on_set_pairs_btn_clicked();

    void on_get_hash_btn_clicked();

    void on_set_hash_btn_clicked();

    void on_get_list_clicked();

    void on_list_addl_clicked();

    void on_list_addr_clicked();

    void on_list_delr_clicked();

    void on_list_dell_clicked();

private:
    Ui::MainWindow *ui;
    QNetworkAccessManager* manager;
    QNetworkReply * send_Request(QNetworkRequest Req, MainWindow::Type type, QString body);
};
#endif // MAINWINDOW_H
