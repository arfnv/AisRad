#/**********************************************************************************
#    Copyright (C) 2013 AIS Foundation.
#
#    This program is free software: you can redistribute it and/or modify
#
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#***********************************************************************************/

include(../aisdev.prf)
INCLUDEPATH += ../include 
DESTDIR = ../lib 
CONFIG += warn_on qt thread staticlib exceptions rtti
TEMPLATE = lib 
HEADERS += amainwindow.h
SOURCES += amainwindow.cpp
CONFIG += build_all debug_and_release
CONFIG(debug, debug|release) {
    TARGET = radguiD
} else {
    TARGET = radgui
}

