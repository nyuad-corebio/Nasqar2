
rm ~/id_rsa
rm id_rsa
cp $4  ~/id_rsa

chmod 400  ~/id_rsa
cp $4  id_rsa
chmod 400  id_rsa

cat << EOF > config
Host test 
    HostName  $2
    User $1
    PubKeyAuthentication yes
    IdentityFile ~/id_rsa
EOF

mv config ~/.ssh/config

umount mnt > /dev/null
rmdir mnt
mkdir mnt > /dev/null
sshfs  -o StrictHostKeyChecking=no  test:$3 mnt 

ls mnt




