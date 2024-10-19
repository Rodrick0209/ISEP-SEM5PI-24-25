using System;
using System.ComponentModel.DataAnnotations;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Utils;
using Microsoft.AspNetCore.Identity;

namespace DDDSample1.Domain.User
{
    public class User : Entity<UserId>, IAggregateRoot
    {
        public Email email{get; private set;}

        public Role role{get; private set;}

        public Password password { get; private set; }

        public ResetPasswordToken ? resetPasswordToken { get; private set; }
        public ResetPasswordTokenExpiration ? resetPasswordTokenExpiration { get; private set; }

        public LoginFailCounter ? loginFailCounter { get; private set; }

        public AccountBlockedTime ? accountBlockedTime { get; private set; }

        public AccountConfirmed ? accountConfirmed { get; private set; }
        public ConfirmationRegisterPatientToken ? confirmationRegisterPatientToken { get; private set; }
        public ConfirmationRegisterPatientTokenExpiration ? confirmationRegisterPatientTokenExpiration { get; private set; }
        private User()
        {
        }

        public User (string email, string role)
        {
            this.Id = new UserId(Guid.NewGuid());
            this.email = new Email(email);
            this.role = new Role(role);
            this.loginFailCounter = new LoginFailCounter(0);
        }

        public User(string email, string role, string password){
            this.Id = new UserId(Guid.NewGuid());
            this.email = new Email(email);
            this.role = new Role(role);
            this.password = new Password(password);
            this.loginFailCounter = new LoginFailCounter(0);
            this.accountConfirmed = new AccountConfirmed(false);
        }

        public void SetPassword(string password)
        {
            this.password = new Password(password);
        }

        public void SetResetPasswordToken(string resetPasswordToken, DateTime expirationDate)
        {
            this.resetPasswordToken = new ResetPasswordToken(resetPasswordToken);
            this.resetPasswordTokenExpiration = new ResetPasswordTokenExpiration(expirationDate);
        }

        public void ClearResetPasswordToken()
        {
            this.resetPasswordToken = null;
            this.resetPasswordTokenExpiration = null;
        }

        public int compareExpirationToken()
        {
            return DateTime.Compare(this.resetPasswordTokenExpiration.resetPasswordTokenExpiration, DateTime.UtcNow);
        }


        //increases the fail counter
        public void IncreaseFailCounter(int maxAllowedFailCounter, int minDefinedAsLimit)
        {
            this.loginFailCounter = new LoginFailCounter(this.loginFailCounter.loginFailCounter + 1);
            if (this.loginFailCounter.loginFailCounter >= maxAllowedFailCounter)
            {
                this.accountBlockedTime = new AccountBlockedTime(DateTime.Now.AddMinutes(minDefinedAsLimit));
                this.loginFailCounter = new LoginFailCounter(0);
            }
        }
        //resets the fail counters
        public void ResetFailCounter()
        {
            this.loginFailCounter = null;
        }

        //makes the account blocked for a certain amount of time
        public void SetAccountBlockedTime(int minDefinedAsLimit)
        {
            this.accountBlockedTime = new AccountBlockedTime(DateTime.Now.AddMinutes(minDefinedAsLimit));
        }   

        //clean the accountBlockedTime
        public void ClearAccountBlockedTime()
        {
            this.accountBlockedTime = null;
        }

        public bool checkIfAccountIsBlocked()
        {
            if (this.accountBlockedTime != null)
            {
                Console.WriteLine("Diferença resul = " + DateTime.Compare(this.accountBlockedTime.accountBlockedTime, DateTime.Now));
            }

            if (this.accountBlockedTime == null || DateTime.Compare(this.accountBlockedTime.accountBlockedTime, DateTime.Now) <= 0)
            {
                Console.WriteLine("Account is not blocked or block time has expired.");
                ClearAccountBlockedTime();
                return false;
            }
            else
            {
                Console.WriteLine("Account is blocked.");
                return true;
            }
        }

        public void SetConfirmationRegisterPatientToken(string token, DateTime expirationDate)
        {
            this.confirmationRegisterPatientToken = new ConfirmationRegisterPatientToken(token);
            this.confirmationRegisterPatientTokenExpiration = new ConfirmationRegisterPatientTokenExpiration(expirationDate);
        }

        public void ConfirmAccount()
        {
            this.accountConfirmed = new AccountConfirmed(true);
            this.confirmationRegisterPatientToken = null;
            this.confirmationRegisterPatientTokenExpiration = null;
        }
            

    }


}