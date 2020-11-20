#!/usr/bin/env python

import sys
import gi

cacheDir = "/home/liolin/.cache/cmus-notify/cover"

def parseCmusArgs():
    length = len(sys.argv)
    args = {};
    for i in range(1, length-1, 2):
        args[sys.argv[i]] = sys.argv[i+1]

    return args


def calculateDuration():
    pass

def extractImage(inputFile, artist, album, title):
    import ffmpeg
    import os

    outputFile = "%s/%s-%s-%s.png" % (cacheDir, artist, album, title);
    if not os.path.isfile(outputFile):
        stream = ffmpeg.input(inputFile)
        stream = ffmpeg.output(stream, outputFile)
        ffmpeg.run(stream)
    return outputFile


def sendNotification(title, outputString = '', cover='/home/liolin/.cache/cmus-notify/cover/default.png'):
    gi.require_version('Notify', '0.7')
    from gi.repository import Notify, GdkPixbuf
    Notify.init("cmus")
    cmusNotification = Notify.Notification.new(title, outputString)
    image = GdkPixbuf.Pixbuf.new_from_file(cover)
    cmusNotification.set_image_from_pixbuf(image)
    cmusNotification.show()
    Notify.uninit()

def main():
    args = parseCmusArgs()

    title = ""
    message = ""

    if "file" in args:
        cover = extractImage(args["file"], args["artist"], args["album"], args["title"])
        title = "[%s]" % args["status"]
        message = "%s\n%s - %s" % (args["title"], args["artist"], args["album"])

    elif "url" in args:
        title = "[%s]" % args["status"]
        message = "%s - %s" % (args["url"], args["title"])

    else:
        title = "[%s]" % args["status"]
        message = ""

    sendNotification(title, message, cover)

if __name__ == "__main__":
    main()
