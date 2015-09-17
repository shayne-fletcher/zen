PREFIXDIR="$HOME/opt"
../gcc-5.2.0/configure --prefix=$PREFIXDIR --enable-languages=c,c++,fortran,go,java,lto,objc,obj-c++ --disable-bootstrap \
--enable-shared \
--build=x86_64-linux-gnu --host=x86_64-linux-gnu --target=x86_64-linux-gnu \
--disable-multilib \
--with-gxx=include-dir=$PREFIXDIR/include/c++/5.2.0 --libdir=$PREFIXDIR/lib
make
rm -rf $PREFIXDIR
make install
