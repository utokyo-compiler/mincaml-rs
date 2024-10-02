// auto-generated: "lalrpop 0.21.0"
// sha3: 544c60d6374b21ea1223d3e5f116cdd92c37cf3291fc5f3ee14e345fa583dcdb
use sourcemap::Loc;
use syntax::{*, IntBinOpKind::*, FloatBinOpKind::*, BooleanBinOpKind::*, mli};
use crate::lexer::{Error as LexError, Token};
use super::Allocator;
#[allow(unused_extern_crates)]
extern crate lalrpop_util as __lalrpop_util;
#[allow(unused_imports)]
use self::__lalrpop_util::state_machine as __state_machine;
#[allow(unused_extern_crates)]
extern crate alloc;

#[rustfmt::skip]
#[allow(explicit_outlives_requirements, non_snake_case, non_camel_case_types, unused_mut, unused_variables, unused_imports, unused_parens, clippy::needless_lifetimes, clippy::type_complexity, clippy::needless_return, clippy::too_many_arguments, clippy::never_loop, clippy::match_single_binding, clippy::needless_raw_string_hashes)]
mod __parse__Mli {

    use sourcemap::Loc;
    use syntax::{*, IntBinOpKind::*, FloatBinOpKind::*, BooleanBinOpKind::*, mli};
    use crate::lexer::{Error as LexError, Token};
    use super::super::Allocator;
    #[allow(unused_extern_crates)]
    extern crate lalrpop_util as __lalrpop_util;
    #[allow(unused_imports)]
    use self::__lalrpop_util::state_machine as __state_machine;
    #[allow(unused_extern_crates)]
    extern crate alloc;
    use super::__ToTriple;
    #[allow(dead_code)]
    pub(crate) enum __Symbol<'input, 'ctx>
     {
        Variant0(Token<'input>),
        Variant1(bool),
        Variant2(i32),
        Variant3(f32),
        Variant4(&'input str),
        Variant5(alloc::vec::Vec<Token<'input>>),
        Variant6(BinOp),
        Variant7(mli::Declaration<'ctx>),
        Variant8(mli::ItemIdent<'ctx>),
        Variant9(mli::Mli<'ctx>),
        Variant10(mli::Operator),
        Variant11(mli::AscribedTy<'ctx>),
    }
    const __ACTION: &[i8] = &[
        // State 0
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 3, 0, 0, 0, 0, 0, 0, 0,
        // State 1
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 3, 0, 0, 0, 0, 0, 0, 0,
        // State 2
        5, 0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 3
        5, 0, 0, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 4
        0, 0, 0, 0, 0, 0, 19, 17, 15, 21, 20, 18, 16, 22, 26, 25, 24, 28, 23, 27, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 5
        0, 0, 0, 0, 0, 30, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 6
        0, 0, 0, 0, 0, 30, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 7
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 0, 0,
        // State 8
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -22, -22, 0, 0, 0, 0, 0, 0, 0,
        // State 9
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -21, -21, 0, 0, 0, 0, 0, 0, 0,
        // State 10
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0,
        // State 11
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -19, 0,
        // State 12
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0,
        // State 13
        0, 31, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 14
        0, -25, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 15
        0, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 16
        0, -23, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 17
        0, -27, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 18
        0, -24, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 19
        0, -28, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 20
        0, -26, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 21
        0, -30, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 22
        0, -35, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 23
        0, -32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 24
        0, -34, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 25
        0, -31, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 26
        0, -36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 27
        0, -33, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 28
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 33, 0, 0, 0,
        // State 29
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -38, 0, 0, 0, 0, 0, 0, -38, -38, 0, 0, 0, -38, 0, 0, 0,
        // State 30
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -20, 0,
        // State 31
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -18, -18, 0, 0, 0, 33, 0, 0, 0,
        // State 32
        0, 0, 0, 0, 0, 34, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        // State 33
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -37, 0, 0, 0, 0, 0, 0, -37, -37, 0, 0, 0, -37, 0, 0, 0,
        // State 34
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -17, -17, 0, 0, 0, 0, 37, 0, 0,
        // State 35
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, 0, 0, 0, 0, -1, 0, 0,
        // State 36
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -2, -2, 0, 0, 0, 0, -2, 0, 0,
    ];
    fn __action(state: i8, integer: usize) -> i8 {
        __ACTION[(state as usize) * 30 + integer]
    }
    const __EOF_ACTION: &[i8] = &[
        // State 0
        0,
        // State 1
        -39,
        // State 2
        0,
        // State 3
        0,
        // State 4
        0,
        // State 5
        0,
        // State 6
        0,
        // State 7
        0,
        // State 8
        -22,
        // State 9
        -21,
        // State 10
        0,
        // State 11
        0,
        // State 12
        0,
        // State 13
        0,
        // State 14
        0,
        // State 15
        0,
        // State 16
        0,
        // State 17
        0,
        // State 18
        0,
        // State 19
        0,
        // State 20
        0,
        // State 21
        0,
        // State 22
        0,
        // State 23
        0,
        // State 24
        0,
        // State 25
        0,
        // State 26
        0,
        // State 27
        0,
        // State 28
        0,
        // State 29
        -38,
        // State 30
        0,
        // State 31
        -18,
        // State 32
        0,
        // State 33
        -37,
        // State 34
        -17,
        // State 35
        -1,
        // State 36
        -2,
    ];
    fn __goto(state: i8, nt: usize) -> i8 {
        match nt {
            0 => 34,
            2 => match state {
                1 => 9,
                _ => 8,
            },
            3 => match state {
                3 => 12,
                _ => 10,
            },
            4 => 1,
            5 => 13,
            6 => match state {
                6 => 31,
                _ => 28,
            },
            _ => 0,
        }
    }
    const __TERMINAL: &[&str] = &[
        r###""(""###,
        r###"")""###,
        r###"Bool"###,
        r###"Int"###,
        r###"Float"###,
        r###"Ident"###,
        r###""-""###,
        r###""+""###,
        r###""*""###,
        r###""/""###,
        r###""-.""###,
        r###""+.""###,
        r###""*.""###,
        r###""/.""###,
        r###""=""###,
        r###""<>""###,
        r###""<=""###,
        r###"">=""###,
        r###""<""###,
        r###"">""###,
        r###""rec""###,
        r###""val""###,
        r###""external""###,
        r###"",""###,
        r###"".""###,
        r###""<-""###,
        r###""->""###,
        r###""%something""###,
        r###"":""###,
        r###"";""###,
    ];
    fn __expected_tokens(__state: i8) -> alloc::vec::Vec<alloc::string::String> {
        __TERMINAL.iter().enumerate().filter_map(|(index, terminal)| {
            let next_state = __action(__state, index);
            if next_state == 0 {
                None
            } else {
                Some(alloc::string::ToString::to_string(terminal))
            }
        }).collect()
    }
    fn __expected_tokens_from_states<
        'input,
        'ctx,
    >(
        __states: &[i8],
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> alloc::vec::Vec<alloc::string::String>
    {
        __TERMINAL.iter().enumerate().filter_map(|(index, terminal)| {
            if __accepts(None, __states, Some(index), core::marker::PhantomData::<(&(), &())>) {
                Some(alloc::string::ToString::to_string(terminal))
            } else {
                None
            }
        }).collect()
    }
    struct __StateMachine<'input, 'ctx>
    where 
    {
        alloc: Allocator<'ctx>,
        __phantom: core::marker::PhantomData<(&'input (), &'ctx ())>,
    }
    impl<'input, 'ctx> __state_machine::ParserDefinition for __StateMachine<'input, 'ctx>
    where 
    {
        type Location = Loc;
        type Error = LexError<'input>;
        type Token = Token<'input>;
        type TokenIndex = usize;
        type Symbol = __Symbol<'input, 'ctx>;
        type Success = mli::Mli<'ctx>;
        type StateIndex = i8;
        type Action = i8;
        type ReduceIndex = i8;
        type NonterminalIndex = usize;

        #[inline]
        fn start_location(&self) -> Self::Location {
              Default::default()
        }

        #[inline]
        fn start_state(&self) -> Self::StateIndex {
              0
        }

        #[inline]
        fn token_to_index(&self, token: &Self::Token) -> Option<usize> {
            __token_to_integer(token, core::marker::PhantomData::<(&(), &())>)
        }

        #[inline]
        fn action(&self, state: i8, integer: usize) -> i8 {
            __action(state, integer)
        }

        #[inline]
        fn error_action(&self, state: i8) -> i8 {
            __action(state, 30 - 1)
        }

        #[inline]
        fn eof_action(&self, state: i8) -> i8 {
            __EOF_ACTION[state as usize]
        }

        #[inline]
        fn goto(&self, state: i8, nt: usize) -> i8 {
            __goto(state, nt)
        }

        fn token_to_symbol(&self, token_index: usize, token: Self::Token) -> Self::Symbol {
            __token_to_symbol(token_index, token, core::marker::PhantomData::<(&(), &())>)
        }

        fn expected_tokens(&self, state: i8) -> alloc::vec::Vec<alloc::string::String> {
            __expected_tokens(state)
        }

        fn expected_tokens_from_states(&self, states: &[i8]) -> alloc::vec::Vec<alloc::string::String> {
            __expected_tokens_from_states(states, core::marker::PhantomData::<(&(), &())>)
        }

        #[inline]
        fn uses_error_recovery(&self) -> bool {
            false
        }

        #[inline]
        fn error_recovery_symbol(
            &self,
            recovery: __state_machine::ErrorRecovery<Self>,
        ) -> Self::Symbol {
            panic!("error recovery not enabled for this grammar")
        }

        fn reduce(
            &mut self,
            action: i8,
            start_location: Option<&Self::Location>,
            states: &mut alloc::vec::Vec<i8>,
            symbols: &mut alloc::vec::Vec<__state_machine::SymbolTriple<Self>>,
        ) -> Option<__state_machine::ParseResult<Self>> {
            __reduce(
                self.alloc,
                action,
                start_location,
                states,
                symbols,
                core::marker::PhantomData::<(&(), &())>,
            )
        }

        fn simulate_reduce(&self, action: i8) -> __state_machine::SimulatedReduce<Self> {
            __simulate_reduce(action, core::marker::PhantomData::<(&(), &())>)
        }
    }
    fn __token_to_integer<
        'input,
        'ctx,
    >(
        __token: &Token<'input>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> Option<usize>
    {
        match __token {
            Token::LPar if true => Some(0),
            Token::RPar if true => Some(1),
            Token::Bool(_) if true => Some(2),
            Token::Int(_) if true => Some(3),
            Token::Float(_) if true => Some(4),
            Token::Ident(_) if true => Some(5),
            Token::Hyphen if true => Some(6),
            Token::Plus if true => Some(7),
            Token::Ast if true => Some(8),
            Token::Slash if true => Some(9),
            Token::HyphenDot if true => Some(10),
            Token::PlusDot if true => Some(11),
            Token::AstDot if true => Some(12),
            Token::SlashDot if true => Some(13),
            Token::Equal if true => Some(14),
            Token::LessGreater if true => Some(15),
            Token::LessEqual if true => Some(16),
            Token::GreaterEqual if true => Some(17),
            Token::Less if true => Some(18),
            Token::Greater if true => Some(19),
            Token::Rec if true => Some(20),
            Token::Val if true => Some(21),
            Token::External if true => Some(22),
            Token::Comma if true => Some(23),
            Token::Dot if true => Some(24),
            Token::LessHyphen if true => Some(25),
            Token::HyphenGreater if true => Some(26),
            Token::StringLiteral if true => Some(27),
            Token::Colon if true => Some(28),
            Token::Semi if true => Some(29),
            _ => None,
        }
    }
    fn __token_to_symbol<
        'input,
        'ctx,
    >(
        __token_index: usize,
        __token: Token<'input>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> __Symbol<'input, 'ctx>
    {
        #[allow(clippy::manual_range_patterns)]match __token_index {
            0 | 1 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 => __Symbol::Variant0(__token),
            2 => match __token {
                Token::Bool(__tok0) if true => __Symbol::Variant1(__tok0),
                _ => unreachable!(),
            },
            3 => match __token {
                Token::Int(__tok0) if true => __Symbol::Variant2(__tok0),
                _ => unreachable!(),
            },
            4 => match __token {
                Token::Float(__tok0) if true => __Symbol::Variant3(__tok0),
                _ => unreachable!(),
            },
            5 => match __token {
                Token::Ident(__tok0) if true => __Symbol::Variant4(__tok0),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }
    fn __simulate_reduce<
        'input,
        'ctx,
    >(
        __reduce_index: i8,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> __state_machine::SimulatedReduce<__StateMachine<'input, 'ctx>>
    {
        match __reduce_index {
            0 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 0,
                }
            }
            1 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 2,
                    nonterminal_produced: 0,
                }
            }
            2 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 1,
                }
            }
            3 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 1,
                }
            }
            4 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 1,
                }
            }
            5 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 1,
                }
            }
            6 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 1,
                }
            }
            7 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 1,
                }
            }
            8 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 1,
                }
            }
            9 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 1,
                }
            }
            10 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 1,
                }
            }
            11 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 1,
                }
            }
            12 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 1,
                }
            }
            13 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 1,
                }
            }
            14 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 1,
                }
            }
            15 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 1,
                }
            }
            16 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 6,
                    nonterminal_produced: 2,
                }
            }
            17 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 4,
                    nonterminal_produced: 2,
                }
            }
            18 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 3,
                }
            }
            19 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 3,
                    nonterminal_produced: 3,
                }
            }
            20 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 2,
                    nonterminal_produced: 4,
                }
            }
            21 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 4,
                }
            }
            22 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 5,
                }
            }
            23 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 5,
                }
            }
            24 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 5,
                }
            }
            25 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 5,
                }
            }
            26 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 5,
                }
            }
            27 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 5,
                }
            }
            28 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 5,
                }
            }
            29 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 5,
                }
            }
            30 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 5,
                }
            }
            31 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 5,
                }
            }
            32 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 5,
                }
            }
            33 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 5,
                }
            }
            34 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 5,
                }
            }
            35 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 5,
                }
            }
            36 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 3,
                    nonterminal_produced: 6,
                }
            }
            37 => {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop: 1,
                    nonterminal_produced: 6,
                }
            }
            38 => __state_machine::SimulatedReduce::Accept,
            _ => panic!("invalid reduction index {}", __reduce_index)
        }
    }
    pub struct MliParser {
        _priv: (),
    }

    impl Default for MliParser { fn default() -> Self { Self::new() } }
    impl MliParser {
        pub fn new() -> MliParser {
            MliParser {
                _priv: (),
            }
        }

        #[allow(dead_code)]
        pub fn parse<
            'input,
            'ctx,
            __TOKEN: __ToTriple<'input, 'ctx, >,
            __TOKENS: IntoIterator<Item=__TOKEN>,
        >(
            &self,
            alloc: Allocator<'ctx>,
            __tokens0: __TOKENS,
        ) -> Result<mli::Mli<'ctx>, __lalrpop_util::ParseError<Loc, Token<'input>, LexError<'input>>>
        {
            let __tokens = __tokens0.into_iter();
            let mut __tokens = __tokens.map(|t| __ToTriple::to_triple(t));
            __state_machine::Parser::drive(
                __StateMachine {
                    alloc,
                    __phantom: core::marker::PhantomData::<(&(), &())>,
                },
                __tokens,
            )
        }
    }
    fn __accepts<
        'input,
        'ctx,
    >(
        __error_state: Option<i8>,
        __states: &[i8],
        __opt_integer: Option<usize>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> bool
    {
        let mut __states = __states.to_vec();
        __states.extend(__error_state);
        loop {
            let mut __states_len = __states.len();
            let __top = __states[__states_len - 1];
            let __action = match __opt_integer {
                None => __EOF_ACTION[__top as usize],
                Some(__integer) => __action(__top, __integer),
            };
            if __action == 0 { return false; }
            if __action > 0 { return true; }
            let (__to_pop, __nt) = match __simulate_reduce(-(__action + 1), core::marker::PhantomData::<(&(), &())>) {
                __state_machine::SimulatedReduce::Reduce {
                    states_to_pop, nonterminal_produced
                } => (states_to_pop, nonterminal_produced),
                __state_machine::SimulatedReduce::Accept => return true,
            };
            __states_len -= __to_pop;
            __states.truncate(__states_len);
            let __top = __states[__states_len - 1];
            let __next_state = __goto(__top, __nt);
            __states.push(__next_state);
        }
    }
    fn __reduce<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __action: i8,
        __lookahead_start: Option<&Loc>,
        __states: &mut alloc::vec::Vec<i8>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> Option<Result<mli::Mli<'ctx>,__lalrpop_util::ParseError<Loc, Token<'input>, LexError<'input>>>>
    {
        let (__pop_states, __nonterminal) = match __action {
            0 => {
                __reduce0(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            1 => {
                __reduce1(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            2 => {
                __reduce2(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            3 => {
                __reduce3(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            4 => {
                __reduce4(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            5 => {
                __reduce5(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            6 => {
                __reduce6(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            7 => {
                __reduce7(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            8 => {
                __reduce8(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            9 => {
                __reduce9(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            10 => {
                __reduce10(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            11 => {
                __reduce11(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            12 => {
                __reduce12(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            13 => {
                __reduce13(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            14 => {
                __reduce14(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            15 => {
                __reduce15(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            16 => {
                __reduce16(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            17 => {
                __reduce17(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            18 => {
                __reduce18(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            19 => {
                __reduce19(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            20 => {
                __reduce20(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            21 => {
                __reduce21(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            22 => {
                __reduce22(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            23 => {
                __reduce23(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            24 => {
                __reduce24(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            25 => {
                __reduce25(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            26 => {
                __reduce26(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            27 => {
                __reduce27(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            28 => {
                __reduce28(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            29 => {
                __reduce29(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            30 => {
                __reduce30(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            31 => {
                __reduce31(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            32 => {
                __reduce32(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            33 => {
                __reduce33(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            34 => {
                __reduce34(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            35 => {
                __reduce35(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            36 => {
                __reduce36(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            37 => {
                __reduce37(alloc, __lookahead_start, __symbols, core::marker::PhantomData::<(&(), &())>)
            }
            38 => {
                // __Mli = Mli => ActionFn(0);
                let __sym0 = __pop_Variant9(__symbols);
                let __start = __sym0.0;
                let __end = __sym0.2;
                let __nt = super::__action0::<>(alloc, __sym0);
                return Some(Ok(__nt));
            }
            _ => panic!("invalid action code {}", __action)
        };
        let __states_len = __states.len();
        __states.truncate(__states_len - __pop_states);
        let __state = *__states.last().unwrap();
        let __next_state = __goto(__state, __nonterminal);
        __states.push(__next_state);
        None
    }
    #[inline(never)]
    fn __symbol_type_mismatch() -> ! {
        panic!("symbol type mismatch")
    }
    fn __pop_Variant6<
      'input,
      'ctx,
    >(
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>
    ) -> (Loc, BinOp, Loc)
     {
        match __symbols.pop() {
            Some((__l, __Symbol::Variant6(__v), __r)) => (__l, __v, __r),
            _ => __symbol_type_mismatch()
        }
    }
    fn __pop_Variant0<
      'input,
      'ctx,
    >(
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>
    ) -> (Loc, Token<'input>, Loc)
     {
        match __symbols.pop() {
            Some((__l, __Symbol::Variant0(__v), __r)) => (__l, __v, __r),
            _ => __symbol_type_mismatch()
        }
    }
    fn __pop_Variant5<
      'input,
      'ctx,
    >(
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>
    ) -> (Loc, alloc::vec::Vec<Token<'input>>, Loc)
     {
        match __symbols.pop() {
            Some((__l, __Symbol::Variant5(__v), __r)) => (__l, __v, __r),
            _ => __symbol_type_mismatch()
        }
    }
    fn __pop_Variant1<
      'input,
      'ctx,
    >(
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>
    ) -> (Loc, bool, Loc)
     {
        match __symbols.pop() {
            Some((__l, __Symbol::Variant1(__v), __r)) => (__l, __v, __r),
            _ => __symbol_type_mismatch()
        }
    }
    fn __pop_Variant3<
      'input,
      'ctx,
    >(
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>
    ) -> (Loc, f32, Loc)
     {
        match __symbols.pop() {
            Some((__l, __Symbol::Variant3(__v), __r)) => (__l, __v, __r),
            _ => __symbol_type_mismatch()
        }
    }
    fn __pop_Variant2<
      'input,
      'ctx,
    >(
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>
    ) -> (Loc, i32, Loc)
     {
        match __symbols.pop() {
            Some((__l, __Symbol::Variant2(__v), __r)) => (__l, __v, __r),
            _ => __symbol_type_mismatch()
        }
    }
    fn __pop_Variant11<
      'input,
      'ctx,
    >(
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>
    ) -> (Loc, mli::AscribedTy<'ctx>, Loc)
     {
        match __symbols.pop() {
            Some((__l, __Symbol::Variant11(__v), __r)) => (__l, __v, __r),
            _ => __symbol_type_mismatch()
        }
    }
    fn __pop_Variant7<
      'input,
      'ctx,
    >(
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>
    ) -> (Loc, mli::Declaration<'ctx>, Loc)
     {
        match __symbols.pop() {
            Some((__l, __Symbol::Variant7(__v), __r)) => (__l, __v, __r),
            _ => __symbol_type_mismatch()
        }
    }
    fn __pop_Variant8<
      'input,
      'ctx,
    >(
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>
    ) -> (Loc, mli::ItemIdent<'ctx>, Loc)
     {
        match __symbols.pop() {
            Some((__l, __Symbol::Variant8(__v), __r)) => (__l, __v, __r),
            _ => __symbol_type_mismatch()
        }
    }
    fn __pop_Variant9<
      'input,
      'ctx,
    >(
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>
    ) -> (Loc, mli::Mli<'ctx>, Loc)
     {
        match __symbols.pop() {
            Some((__l, __Symbol::Variant9(__v), __r)) => (__l, __v, __r),
            _ => __symbol_type_mismatch()
        }
    }
    fn __pop_Variant10<
      'input,
      'ctx,
    >(
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>
    ) -> (Loc, mli::Operator, Loc)
     {
        match __symbols.pop() {
            Some((__l, __Symbol::Variant10(__v), __r)) => (__l, __v, __r),
            _ => __symbol_type_mismatch()
        }
    }
    fn __pop_Variant4<
      'input,
      'ctx,
    >(
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>
    ) -> (Loc, &'input str, Loc)
     {
        match __symbols.pop() {
            Some((__l, __Symbol::Variant4(__v), __r)) => (__l, __v, __r),
            _ => __symbol_type_mismatch()
        }
    }
    fn __reduce0<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // "%something"+ = "%something" => ActionFn(24);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action24::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant5(__nt), __end));
        (1, 0)
    }
    fn __reduce1<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // "%something"+ = "%something"+, "%something" => ActionFn(25);
        assert!(__symbols.len() >= 2);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant5(__symbols);
        let __start = __sym0.0;
        let __end = __sym1.2;
        let __nt = super::__action25::<>(alloc, __sym0, __sym1);
        __symbols.push((__start, __Symbol::Variant5(__nt), __end));
        (2, 0)
    }
    fn __reduce2<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // BinOp = "+" => ActionFn(7);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action7::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant6(__nt), __end));
        (1, 1)
    }
    fn __reduce3<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // BinOp = "-" => ActionFn(8);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action8::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant6(__nt), __end));
        (1, 1)
    }
    fn __reduce4<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // BinOp = "*" => ActionFn(9);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action9::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant6(__nt), __end));
        (1, 1)
    }
    fn __reduce5<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // BinOp = "/" => ActionFn(10);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action10::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant6(__nt), __end));
        (1, 1)
    }
    fn __reduce6<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // BinOp = "+." => ActionFn(11);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action11::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant6(__nt), __end));
        (1, 1)
    }
    fn __reduce7<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // BinOp = "-." => ActionFn(12);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action12::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant6(__nt), __end));
        (1, 1)
    }
    fn __reduce8<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // BinOp = "*." => ActionFn(13);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action13::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant6(__nt), __end));
        (1, 1)
    }
    fn __reduce9<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // BinOp = "/." => ActionFn(14);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action14::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant6(__nt), __end));
        (1, 1)
    }
    fn __reduce10<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // BinOp = "=" => ActionFn(15);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action15::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant6(__nt), __end));
        (1, 1)
    }
    fn __reduce11<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // BinOp = "<=" => ActionFn(16);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action16::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant6(__nt), __end));
        (1, 1)
    }
    fn __reduce12<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // BinOp = ">=" => ActionFn(17);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action17::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant6(__nt), __end));
        (1, 1)
    }
    fn __reduce13<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // BinOp = "<>" => ActionFn(18);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action18::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant6(__nt), __end));
        (1, 1)
    }
    fn __reduce14<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // BinOp = "<" => ActionFn(19);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action19::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant6(__nt), __end));
        (1, 1)
    }
    fn __reduce15<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // BinOp = ">" => ActionFn(20);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action20::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant6(__nt), __end));
        (1, 1)
    }
    fn __reduce16<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // Decl = "external", ItemIdent, ":", TypeAscription, "=", "%something"+ => ActionFn(3);
        assert!(__symbols.len() >= 6);
        let __sym5 = __pop_Variant5(__symbols);
        let __sym4 = __pop_Variant0(__symbols);
        let __sym3 = __pop_Variant11(__symbols);
        let __sym2 = __pop_Variant0(__symbols);
        let __sym1 = __pop_Variant8(__symbols);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym5.2;
        let __nt = super::__action3::<>(alloc, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5);
        __symbols.push((__start, __Symbol::Variant7(__nt), __end));
        (6, 2)
    }
    fn __reduce17<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // Decl = "val", ItemIdent, ":", TypeAscription => ActionFn(4);
        assert!(__symbols.len() >= 4);
        let __sym3 = __pop_Variant11(__symbols);
        let __sym2 = __pop_Variant0(__symbols);
        let __sym1 = __pop_Variant8(__symbols);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym3.2;
        let __nt = super::__action4::<>(alloc, __sym0, __sym1, __sym2, __sym3);
        __symbols.push((__start, __Symbol::Variant7(__nt), __end));
        (4, 2)
    }
    fn __reduce18<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // ItemIdent = Ident => ActionFn(5);
        let __sym0 = __pop_Variant4(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action5::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant8(__nt), __end));
        (1, 3)
    }
    fn __reduce19<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // ItemIdent = "(", Op, ")" => ActionFn(6);
        assert!(__symbols.len() >= 3);
        let __sym2 = __pop_Variant0(__symbols);
        let __sym1 = __pop_Variant10(__symbols);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym2.2;
        let __nt = super::__action6::<>(alloc, __sym0, __sym1, __sym2);
        __symbols.push((__start, __Symbol::Variant8(__nt), __end));
        (3, 3)
    }
    fn __reduce20<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // Mli = Mli, Decl => ActionFn(1);
        assert!(__symbols.len() >= 2);
        let __sym1 = __pop_Variant7(__symbols);
        let __sym0 = __pop_Variant9(__symbols);
        let __start = __sym0.0;
        let __end = __sym1.2;
        let __nt = super::__action1::<>(alloc, __sym0, __sym1);
        __symbols.push((__start, __Symbol::Variant9(__nt), __end));
        (2, 4)
    }
    fn __reduce21<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // Mli = Decl => ActionFn(2);
        let __sym0 = __pop_Variant7(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action2::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant9(__nt), __end));
        (1, 4)
    }
    fn __reduce22<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // Op = "+" => ActionFn(26);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action26::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant10(__nt), __end));
        (1, 5)
    }
    fn __reduce23<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // Op = "-" => ActionFn(27);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action27::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant10(__nt), __end));
        (1, 5)
    }
    fn __reduce24<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // Op = "*" => ActionFn(28);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action28::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant10(__nt), __end));
        (1, 5)
    }
    fn __reduce25<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // Op = "/" => ActionFn(29);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action29::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant10(__nt), __end));
        (1, 5)
    }
    fn __reduce26<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // Op = "+." => ActionFn(30);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action30::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant10(__nt), __end));
        (1, 5)
    }
    fn __reduce27<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // Op = "-." => ActionFn(31);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action31::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant10(__nt), __end));
        (1, 5)
    }
    fn __reduce28<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // Op = "*." => ActionFn(32);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action32::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant10(__nt), __end));
        (1, 5)
    }
    fn __reduce29<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // Op = "/." => ActionFn(33);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action33::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant10(__nt), __end));
        (1, 5)
    }
    fn __reduce30<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // Op = "=" => ActionFn(34);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action34::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant10(__nt), __end));
        (1, 5)
    }
    fn __reduce31<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // Op = "<=" => ActionFn(35);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action35::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant10(__nt), __end));
        (1, 5)
    }
    fn __reduce32<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // Op = ">=" => ActionFn(36);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action36::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant10(__nt), __end));
        (1, 5)
    }
    fn __reduce33<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // Op = "<>" => ActionFn(37);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action37::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant10(__nt), __end));
        (1, 5)
    }
    fn __reduce34<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // Op = "<" => ActionFn(38);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action38::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant10(__nt), __end));
        (1, 5)
    }
    fn __reduce35<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // Op = ">" => ActionFn(39);
        let __sym0 = __pop_Variant0(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action39::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant10(__nt), __end));
        (1, 5)
    }
    fn __reduce36<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // TypeAscription = TypeAscription, "->", Ident => ActionFn(22);
        assert!(__symbols.len() >= 3);
        let __sym2 = __pop_Variant4(__symbols);
        let __sym1 = __pop_Variant0(__symbols);
        let __sym0 = __pop_Variant11(__symbols);
        let __start = __sym0.0;
        let __end = __sym2.2;
        let __nt = super::__action22::<>(alloc, __sym0, __sym1, __sym2);
        __symbols.push((__start, __Symbol::Variant11(__nt), __end));
        (3, 6)
    }
    fn __reduce37<
        'input,
        'ctx,
    >(
        alloc: Allocator<'ctx>,
        __lookahead_start: Option<&Loc>,
        __symbols: &mut alloc::vec::Vec<(Loc,__Symbol<'input, 'ctx>,Loc)>,
        _: core::marker::PhantomData<(&'input (), &'ctx ())>,
    ) -> (usize, usize)
    {
        // TypeAscription = Ident => ActionFn(23);
        let __sym0 = __pop_Variant4(__symbols);
        let __start = __sym0.0;
        let __end = __sym0.2;
        let __nt = super::__action23::<>(alloc, __sym0);
        __symbols.push((__start, __Symbol::Variant11(__nt), __end));
        (1, 6)
    }
}
#[allow(unused_imports)]
pub use self::__parse__Mli::MliParser;

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action0<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, __0, _): (Loc, mli::Mli<'ctx>, Loc),
) -> mli::Mli<'ctx>
{
    __0
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action1<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, mut a, _): (Loc, mli::Mli<'ctx>, Loc),
    (_, d, _): (Loc, mli::Declaration<'ctx>, Loc),
) -> mli::Mli<'ctx>
{
    {
        a.declarations.push(d);
        a
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action2<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, __0, _): (Loc, mli::Declaration<'ctx>, Loc),
) -> mli::Mli<'ctx>
{
    mli::Mli { declarations: vec![__0]}
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action3<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, _, _): (Loc, Token<'input>, Loc),
    (_, item_ident, _): (Loc, mli::ItemIdent<'ctx>, Loc),
    (_, _, _): (Loc, Token<'input>, Loc),
    (_, ascribed_ty, _): (Loc, mli::AscribedTy<'ctx>, Loc),
    (_, _, _): (Loc, Token<'input>, Loc),
    (_, _, _): (Loc, alloc::vec::Vec<Token<'input>>, Loc),
) -> mli::Declaration<'ctx>
{
    mli::Declaration {
            item_ident,
            ascribed_ty,
        }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action4<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, _, _): (Loc, Token<'input>, Loc),
    (_, item_ident, _): (Loc, mli::ItemIdent<'ctx>, Loc),
    (_, _, _): (Loc, Token<'input>, Loc),
    (_, ascribed_ty, _): (Loc, mli::AscribedTy<'ctx>, Loc),
) -> mli::Declaration<'ctx>
{
    mli::Declaration {
            item_ident,
            ascribed_ty,
        }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action5<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, x, _): (Loc, &'input str, Loc),
) -> mli::ItemIdent<'ctx>
{
    mli::ItemIdent::new_ident(alloc.ctx().intern_ident(x))
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action6<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, _, _): (Loc, Token<'input>, Loc),
    (_, op, _): (Loc, mli::Operator, Loc),
    (_, _, _): (Loc, Token<'input>, Loc),
) -> mli::ItemIdent<'ctx>
{
    mli::ItemIdent::new_operator(op)
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action7<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, __0, _): (Loc, Token<'input>, Loc),
) -> BinOp
{
    BinOp::Int(Add)
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action8<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, __0, _): (Loc, Token<'input>, Loc),
) -> BinOp
{
    BinOp::Int(Sub)
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action9<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, __0, _): (Loc, Token<'input>, Loc),
) -> BinOp
{
    BinOp::Int(Mul)
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action10<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, __0, _): (Loc, Token<'input>, Loc),
) -> BinOp
{
    BinOp::Int(Div)
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action11<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, __0, _): (Loc, Token<'input>, Loc),
) -> BinOp
{
    BinOp::Float(FAdd)
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action12<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, __0, _): (Loc, Token<'input>, Loc),
) -> BinOp
{
    BinOp::Float(FSub)
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action13<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, __0, _): (Loc, Token<'input>, Loc),
) -> BinOp
{
    BinOp::Float(FMul)
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action14<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, __0, _): (Loc, Token<'input>, Loc),
) -> BinOp
{
    BinOp::Float(FDiv)
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action15<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, __0, _): (Loc, Token<'input>, Loc),
) -> BinOp
{
    BinOp::Boolean(Eq)
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action16<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, __0, _): (Loc, Token<'input>, Loc),
) -> BinOp
{
    BinOp::Boolean(Le)
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action17<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, __0, _): (Loc, Token<'input>, Loc),
) -> BinOp
{
    BinOp::Boolean(Ge)
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action18<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, __0, _): (Loc, Token<'input>, Loc),
) -> BinOp
{
    BinOp::Boolean(Ne)
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action19<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, __0, _): (Loc, Token<'input>, Loc),
) -> BinOp
{
    BinOp::Boolean(Lt)
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action20<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, __0, _): (Loc, Token<'input>, Loc),
) -> BinOp
{
    BinOp::Boolean(Gt)
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action21<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, __0, _): (Loc, BinOp, Loc),
) -> mli::Operator
{
    mli::Operator::Binary(__0)
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action22<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, mut a, _): (Loc, mli::AscribedTy<'ctx>, Loc),
    (_, _, _): (Loc, Token<'input>, Loc),
    (_, t, _): (Loc, &'input str, Loc),
) -> mli::AscribedTy<'ctx>
{
    {
        a.elements.push(alloc.ctx().intern_ident(t));
        a
    }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action23<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, __0, _): (Loc, &'input str, Loc),
) -> mli::AscribedTy<'ctx>
{
    mli::AscribedTy::new(vec![alloc.ctx().intern_ident(__0)])
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action24<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, __0, _): (Loc, Token<'input>, Loc),
) -> alloc::vec::Vec<Token<'input>>
{
    alloc::vec![__0]
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes, clippy::just_underscores_and_digits)]
fn __action25<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    (_, v, _): (Loc, alloc::vec::Vec<Token<'input>>, Loc),
    (_, e, _): (Loc, Token<'input>, Loc),
) -> alloc::vec::Vec<Token<'input>>
{
    { let mut v = v; v.push(e); v }
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action26<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    __0: (Loc, Token<'input>, Loc),
) -> mli::Operator
{
    let __start0 = __0.0;
    let __end0 = __0.2;
    let __temp0 = __action7(
        alloc,
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action21(
        alloc,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action27<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    __0: (Loc, Token<'input>, Loc),
) -> mli::Operator
{
    let __start0 = __0.0;
    let __end0 = __0.2;
    let __temp0 = __action8(
        alloc,
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action21(
        alloc,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action28<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    __0: (Loc, Token<'input>, Loc),
) -> mli::Operator
{
    let __start0 = __0.0;
    let __end0 = __0.2;
    let __temp0 = __action9(
        alloc,
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action21(
        alloc,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action29<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    __0: (Loc, Token<'input>, Loc),
) -> mli::Operator
{
    let __start0 = __0.0;
    let __end0 = __0.2;
    let __temp0 = __action10(
        alloc,
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action21(
        alloc,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action30<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    __0: (Loc, Token<'input>, Loc),
) -> mli::Operator
{
    let __start0 = __0.0;
    let __end0 = __0.2;
    let __temp0 = __action11(
        alloc,
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action21(
        alloc,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action31<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    __0: (Loc, Token<'input>, Loc),
) -> mli::Operator
{
    let __start0 = __0.0;
    let __end0 = __0.2;
    let __temp0 = __action12(
        alloc,
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action21(
        alloc,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action32<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    __0: (Loc, Token<'input>, Loc),
) -> mli::Operator
{
    let __start0 = __0.0;
    let __end0 = __0.2;
    let __temp0 = __action13(
        alloc,
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action21(
        alloc,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action33<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    __0: (Loc, Token<'input>, Loc),
) -> mli::Operator
{
    let __start0 = __0.0;
    let __end0 = __0.2;
    let __temp0 = __action14(
        alloc,
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action21(
        alloc,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action34<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    __0: (Loc, Token<'input>, Loc),
) -> mli::Operator
{
    let __start0 = __0.0;
    let __end0 = __0.2;
    let __temp0 = __action15(
        alloc,
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action21(
        alloc,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action35<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    __0: (Loc, Token<'input>, Loc),
) -> mli::Operator
{
    let __start0 = __0.0;
    let __end0 = __0.2;
    let __temp0 = __action16(
        alloc,
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action21(
        alloc,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action36<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    __0: (Loc, Token<'input>, Loc),
) -> mli::Operator
{
    let __start0 = __0.0;
    let __end0 = __0.2;
    let __temp0 = __action17(
        alloc,
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action21(
        alloc,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action37<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    __0: (Loc, Token<'input>, Loc),
) -> mli::Operator
{
    let __start0 = __0.0;
    let __end0 = __0.2;
    let __temp0 = __action18(
        alloc,
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action21(
        alloc,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action38<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    __0: (Loc, Token<'input>, Loc),
) -> mli::Operator
{
    let __start0 = __0.0;
    let __end0 = __0.2;
    let __temp0 = __action19(
        alloc,
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action21(
        alloc,
        __temp0,
    )
}

#[allow(unused_variables)]
#[allow(clippy::too_many_arguments, clippy::needless_lifetimes,
    clippy::just_underscores_and_digits)]
fn __action39<
    'input,
    'ctx,
>(
    alloc: Allocator<'ctx>,
    __0: (Loc, Token<'input>, Loc),
) -> mli::Operator
{
    let __start0 = __0.0;
    let __end0 = __0.2;
    let __temp0 = __action20(
        alloc,
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action21(
        alloc,
        __temp0,
    )
}
#[allow(clippy::type_complexity, dead_code)]

pub  trait __ToTriple<'input, 'ctx, >
{
    fn to_triple(value: Self) -> Result<(Loc,Token<'input>,Loc), __lalrpop_util::ParseError<Loc, Token<'input>, LexError<'input>>>;
}

impl<'input, 'ctx, > __ToTriple<'input, 'ctx, > for (Loc, Token<'input>, Loc)
{
    fn to_triple(value: Self) -> Result<(Loc,Token<'input>,Loc), __lalrpop_util::ParseError<Loc, Token<'input>, LexError<'input>>> {
        Ok(value)
    }
}
impl<'input, 'ctx, > __ToTriple<'input, 'ctx, > for Result<(Loc, Token<'input>, Loc), LexError<'input>>
{
    fn to_triple(value: Self) -> Result<(Loc,Token<'input>,Loc), __lalrpop_util::ParseError<Loc, Token<'input>, LexError<'input>>> {
        match value {
            Ok(v) => Ok(v),
            Err(error) => Err(__lalrpop_util::ParseError::User { error }),
        }
    }
}
