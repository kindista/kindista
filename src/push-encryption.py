from __future__ import print_function
import http_ece
import os
import sys
import pyelliptic
import base64

def b64e(arg):
    return base64.urlsafe_b64encode(arg).decode()

def main():
    if len(sys.argv) < 4:
        #print ("usage: python push-encryption.py <client-pub-key> <server-auth> <message>")
        sys.exit(1)

    # generate ephemerial public key using ecdh
    serverECDH = pyelliptic.ECC(curve="prime256v1")
    serverPubKey = b64e(serverECDH.get_pubkey()[1:])

    http_ece.keys[serverPubKey] = serverECDH
    http_ece.labels[serverPubKey] = "P-256"

    salt = os.urandom(16)

    clientPubKey64 = sys.argv[1]
    clientPubKey = base64.urlsafe_b64decode(clientPubKey64)
   
    clientAuthSecret64 = sys.argv[2]
    clientAuthSecret = base64.urlsafe_b64decode(clientAuthSecret64)

    messageRaw = sys.argv[3]
    messageRaw = messageRaw.encode('utf8')
    messageRaw = buffer(messageRaw)


    messageEncrypted = http_ece.encrypt(messageRaw, salt=salt,
                              keyid=serverPubKey, dh=clientPubKey,
                              authSecret=clientAuthSecret)

    print ("%s,%s,%s" % (base64.urlsafe_b64encode(salt), base64.urlsafe_b64encode(serverECDH.get_pubkey()), base64.b64encode(messageEncrypted)), end="")

if __name__ == '__main__':
    main()
