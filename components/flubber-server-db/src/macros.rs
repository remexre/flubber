macro_rules! queries {
    ($($(#[$meta:meta])* fn $name:ident(&$self:ident $(,$arg:ident : $argt:ty)*) -> $outt:ty $body:block)*) => {
        #[allow(non_camel_case_types)]
        enum Query {
            $($name($($argt,)* oneshot::Sender<Result<$outt>>),)*
        }

        impl Database {
            $(
                $(#[$meta])*
                pub async fn $name(&mut self, $($arg : $argt),*) -> Result<$outt> {
                    let (send, recv) = oneshot::channel();
                    let query = Query::$name($($arg,)* send);
                    self.send.send(query)?;
                    recv.await?
                }
            )*
        }

        trait ConnectionExt {
            fn handle_query(&self, query: Query);

            $(fn $name(&self, $($arg : $argt),*) -> Result<$outt>;)*
        }

        impl ConnectionExt for Connection {
            fn handle_query(&self, query: Query) {
                match query {
                    $(Query::$name($($arg,)* send) => {
                        drop(send.send(self.$name($($arg),*)));
                    },)*
                }
            }

            $(fn $name(&$self, $($arg : $argt),*) -> Result<$outt> {
                $body
            })*
        }
    };
}
