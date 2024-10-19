using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.User
{
    public class AccountConfirmed : IValueObject
    {
        public bool Confirmed { get; private set; }

        public AccountConfirmed(bool confirmed)
        {
            this.Confirmed = Confirmed;
        }
    }
}