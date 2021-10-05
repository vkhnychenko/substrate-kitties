#![cfg_attr(not(feature = "std"), no_std)]

pub use pallet::*;

#[frame_support::pallet]
pub mod pallet {
	use frame_support::{sp_runtime::traits::{Hash, Zero},
						dispatch::{DispatchResultWithPostInfo, DispatchResult},
						traits::{Currency, ExistenceRequirement, Randomness},
						pallet_prelude::*};
	use frame_system::pallet_prelude::*;
	use sp_core::H256;

	// Struct for holding Kitty information.
	#[derive(Clone, Encode, Decode, Default, PartialEq)]
	pub struct Kitty<Hash, Balance> {
		id: Hash,
		dna: Hash,
		price: Balance,
		gender: Gender,
	}

	#[derive(Encode, Decode, Debug, Clone, PartialEq)]
	pub enum Gender {
		Male,
		Female,
	}

	impl Default for Gender {
		fn default() -> Self {
			Gender::Male
		}
	}


	#[pallet::pallet]
	#[pallet::generate_store(trait Store)]
	pub struct Pallet<T>(_);

	/// Configure the pallet by specifying the parameters and types it depends on.
	#[pallet::config]
	pub trait Config: pallet_balances::Config + frame_system::Config {
		/// Because this pallet emits events, it depends on the runtime's definition of an event.
		type Event: From<Event<Self>> + IsType<<Self as frame_system::Config>::Event>;

		type KittyRandomness: Randomness<H256, u32>;
	}

	// Errors.
	#[pallet::error]
	pub enum Error<T> {
		// TODO Part III
	}

	//Events.
	#[pallet::event]
	#[pallet::metadata(T::AccountId = "AccountId")]
	#[pallet::generate_deposit(pub(super) fn deposit_event)]
	pub enum Event<T: Config> {
		// TODO Part III
	}

	#[pallet::storage]
	#[pallet::getter(fn all_kitties_count)]
	pub(super) type AllKittiesCount<T: Config> = StorageValue<_, u64, ValueQuery>;

	#[pallet::storage]
	#[pallet::getter(fn get_nonce)]
	pub(super) type Nonce<T: Config> = StorageValue<_, u64, ValueQuery>;

	#[pallet::storage]
	#[pallet::getter(fn kitty)]
	pub(super) type Kitties<T: Config> =
		StorageMap<_, Twox64Concat, T::Hash, Kitty<T::Hash, T::Balance>, ValueQuery>;

	#[pallet::storage]
	#[pallet::getter(fn owner_of)]
	pub(super) type KittyOwner<T: Config> =
		StorageMap<_, Twox64Concat, T::Hash, Kitty<T::Hash, T::Balance>, ValueQuery>;

	// An index to track of all Kitties.
	#[pallet::storage]
	#[pallet::getter(fn kitty_by_index)]
	pub(super) type AllKittiesArray<T: Config> =
		StorageMap<_, Twox64Concat, u64, T::Hash, ValueQuery>;

	// Keeps track of all the Kitties.
	#[pallet::storage]
	pub(super) type AllKittiesIndex<T: Config> =
		StorageMap<_, Twox64Concat, T::Hash, u64, ValueQuery>;

	// Keep track of who a Kitty is owned by.
	#[pallet::storage]
	#[pallet::getter(fn kitty_of_owner_by_index)]
	pub(super) type OwnedKittiesArray<T: Config> =
		StorageMap<_, Twox64Concat, (T::AccountId, u64), T::Hash, ValueQuery>;

	// Keeps track of the total amount of Kitties owned.
	#[pallet::storage]
	#[pallet::getter(fn owned_kitty_count)]
	pub(super) type OwnedKittiesCount<T: Config> =
		StorageMap<_, Twox64Concat, T::AccountId, u64, ValueQuery>;

	// Keeps track of all owned Kitties by index.
	#[pallet::storage]
	pub(super) type OwnedKittiesIndex<T: Config> =
		StorageMap<_, Twox64Concat, T::Hash, u64, ValueQuery>;



	#[pallet::call]
	impl<T: Config> Pallet<T> {

		#[pallet::weight(100)]
		pub fn create_kitty(origin: OriginFor<T>) -> DispatchResultWithPostInfo {
			let sender = ensure_signed(origin)?;
			let random_hash = Self::random_hash(&sender);

			let new_kitty = Kitty {
				id: random_hash,
				dna: random_hash,
				price: 0u8.into(),
				gender: Kitty::<T, T>::gender(random_hash),
			};

			Self::mint(sender, random_hash, new_kitty)?;
			Self::increment_nonce()?;

			Ok(().into())
		}

		// TODO Part III: set_price

		// TODO Part III: transfer

		// TODO Part III: buy_kitty

		// TODO Part III: breed_kitty
	}

	impl<T: Config> Kitty<T, T> {
		pub fn gender(dna: T::Hash) -> Gender {
			if dna.as_ref()[0] % 2 == 0 {
				Gender::Male
			} else {
				Gender::Female
			}
		}
	}

	impl<T: Config> Pallet<T> {
		// TODO Part III: helper functions for dispatchable functions

		fn increment_nonce() -> DispatchResult {
			<Nonce<T>>::try_mutate(|nonce| {
				let next = nonce.checked_add(1).ok_or("Overflow")?; // TODO Part III: Add error handling
				*nonce = next;

				Ok(().into())
			})
		}

		fn random_hash(sender: &T::AccountId) -> T::Hash {
			let nonce = <Nonce<T>>::get();
			let seed = T::KittyRandomness::random_seed();

			T::Hashing::hash_of(&(seed, &sender, nonce))
		}

	}
}