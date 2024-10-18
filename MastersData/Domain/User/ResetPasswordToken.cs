



using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.User
{

public class ResetPasswordToken : IValueObject
{
    public string resetPasswordToken { get; private set; }

    public ResetPasswordToken(string resetPasswordToken)
    {
        if (string.IsNullOrEmpty(resetPasswordToken)) throw new ArgumentException("Reset token cannot be null or empty.");
        this.resetPasswordToken = resetPasswordToken;
    }

}

}