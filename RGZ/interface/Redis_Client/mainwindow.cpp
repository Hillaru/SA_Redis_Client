#include "mainwindow.h"
#include "ui_mainwindow.h"

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    manager = new QNetworkAccessManager(this);
}

MainWindow::~MainWindow()
{
    delete ui;
}

QNetworkReply * MainWindow::send_Request(QNetworkRequest Req, MainWindow::Type type, QString body)
{
    QNetworkReply* reply;
    switch (type) {
        case MainWindow::Type::POST: {
            reply = manager->post(Req, body.toUtf8());
            break;
        } case MainWindow::Type::GET: {
            reply = manager->get(Req);
            break;
        } case MainWindow::Type::DEL: {
            reply = manager->deleteResource(Req);
            break;
        } default:
            reply = nullptr;
            Q_ASSERT(false);
        }
    return reply;
}

void MainWindow::on_pushButton_clicked()
{

}

void MainWindow::replyFinished()
{
  QNetworkReply *reply=
    qobject_cast<QNetworkReply *>(sender());
  if (reply->error() == QNetworkReply::NoError)
  {
    QByteArray content= reply->readAll();
    ui->textEdit->setPlainText(content.data());
  }
  else
  {
    ui->textEdit->setPlainText(reply->errorString());
  }
  reply->deleteLater();
}

void MainWindow::on_get_pairs_btn_clicked()
{
    QStringList list = ui->get_pairs->text().split(", ", Qt::SkipEmptyParts);
    QString opts = "";
    int i;
    for(i = 0; i<list.count(); i++)
    {
        if (i != 0) opts += "&";
        opts += list[i];
    }
    QUrl url("http://localhost:8081/mget?" + opts);
    QNetworkRequest request(url);
    QNetworkReply* reply=  send_Request(request, MainWindow::Type::GET, "");
    connect( reply, SIGNAL(finished()),
             this, SLOT(replyFinished()) );
}


void MainWindow::on_set_pairs_btn_clicked()
{
    QStringList list = ui->set_pairs->text().split(", ", Qt::SkipEmptyParts);
    QString opts = "";
    int i;
    for(i = 0; i<list.count(); i++)
    {
        if (i != 0) opts += "&";
        opts += list[i];
    }
    QUrl url("http://localhost:8081/mset");
    QNetworkRequest request(url);
    QNetworkReply* reply=  send_Request(request, MainWindow::Type::POST, opts);
    connect( reply, SIGNAL(finished()),
             this, SLOT(replyFinished()) );
}


void MainWindow::on_get_hash_btn_clicked()
{
    QString key = ui->hash_key->text();
    QUrl url("http://localhost:8081/hgetall/" + key);
    QNetworkRequest request(url);
    QNetworkReply* reply=  send_Request(request, MainWindow::Type::GET, "");
    connect( reply, SIGNAL(finished()),
             this, SLOT(replyFinished()) );
}


void MainWindow::on_set_hash_btn_clicked()
{
    QString key = ui->hash_key->text();
    QStringList list = ui->set_hash->text().split(", ", Qt::SkipEmptyParts);
    QString opts = "";
    int i;
    for(i = 0; i<list.count(); i++)
    {
        if (i != 0) opts += "&";
        opts += list[i];
    }
    QUrl url("http://localhost:8081/hmset/" + key);
    QNetworkRequest request(url);
    QNetworkReply* reply=  send_Request(request, MainWindow::Type::POST, opts);
    connect( reply, SIGNAL(finished()),
             this, SLOT(replyFinished()) );
}


void MainWindow::on_get_list_clicked()
{
    QString key = ui->list_key->text();
    QUrl url("http://localhost:8081/lrange/" + key);
    QNetworkRequest request(url);
    QNetworkReply* reply=  send_Request(request, MainWindow::Type::GET, "");
    connect( reply, SIGNAL(finished()),
             this, SLOT(replyFinished()) );
}


void MainWindow::on_list_addl_clicked()
{
    QString key = ui->list_key->text();
    QString val = ui->list_val->text();
    QUrl url("http://localhost:8081/lpush/" + key);
    QNetworkRequest request(url);
    QNetworkReply* reply=  send_Request(request, MainWindow::Type::POST, val);
    connect( reply, SIGNAL(finished()),
             this, SLOT(replyFinished()) );
}


void MainWindow::on_list_addr_clicked()
{
    QString key = ui->list_key->text();
    QString val = ui->list_val->text();
    QUrl url("http://localhost:8081/rpush/" + key);
    QNetworkRequest request(url);
    QNetworkReply* reply=  send_Request(request, MainWindow::Type::POST, val);
    connect( reply, SIGNAL(finished()),
             this, SLOT(replyFinished()) );
}


void MainWindow::on_list_delr_clicked()
{
    QString key = ui->list_key->text();
    QUrl url("http://localhost:8081/rpop/" + key);
    QNetworkRequest request(url);
    QNetworkReply* reply=  send_Request(request, MainWindow::Type::DEL, "");
    connect( reply, SIGNAL(finished()),
             this, SLOT(replyFinished()) );
}


void MainWindow::on_list_dell_clicked()
{
    QString key = ui->list_key->text();
    QUrl url("http://localhost:8081/lpop/" + key);
    QNetworkRequest request(url);
    QNetworkReply* reply=  send_Request(request, MainWindow::Type::DEL, "");
    connect( reply, SIGNAL(finished()),
             this, SLOT(replyFinished()) );
}

