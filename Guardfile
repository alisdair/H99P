# A sample Guardfile
# More info at https://github.com/guard/guard#readme

guard :shell do
  watch(/.*\.hs$/) { system "cd src && runhaskell ../spec/*.hs" }
end
