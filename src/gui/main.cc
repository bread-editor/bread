#include <QApplication>
#include <QMainWindow>
#include <QGraphicsScene>
#include <QGraphicsView>
#include <QGridLayout>
#include <QGraphicsTextItem>
#include <QFont>
#include <fstream>
#include <sstream>
#include <string>
#include <cerrno>

int main(int argc, char** argv) {
  std::string contents;
  std::ifstream in("bin/Testfile.txt", std::ios::in | std::ios::binary);

  if(in) {
    in.seekg(0, std::ios::end);
    contents.resize(in.tellg());
    in.seekg(0, std::ios::beg);
    in.read(&contents[0], contents.size());
    in.close();
  }
  
  QApplication bread(argc, argv);

  QMainWindow *mbread = new QMainWindow();
  mbread->resize(800,800);
  mbread->setContentsMargins(0, 0, 0, 0);
  mbread->setWindowTitle("Bread!");

  QFont *font = new QFont();
  font->setPixelSize(12);
  font->setBold(false);
  font->setFamily("Meslo LG S");

  QGraphicsTextItem *io = new QGraphicsTextItem();
  io->setFont(*font);
  io->setPos(0, 0);
  io->setPlainText(contents.c_str());
  io->setTextWidth(500);
  
  QGraphicsScene *scene = new QGraphicsScene;
  scene->addItem(io);

  QGraphicsView *view = new QGraphicsView(scene);
  view->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
  view->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);

  mbread->setCentralWidget(view);
  view->show();
  mbread->show();
  return bread.exec();
}
