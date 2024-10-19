



using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.User
{

public class ResetPasswordTokenExpiration : IValueObject
{
    public DateTime resetPasswordTokenExpiration { get; private set; }

    public ResetPasswordTokenExpiration(DateTime resetPasswordTokenExpiration)
    {
        if (resetPasswordTokenExpiration <= DateTime.UtcNow)
            throw new ArgumentException("Expiration date must be in the future.");

        this.resetPasswordTokenExpiration = resetPasswordTokenExpiration;
    }




}

}