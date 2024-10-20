

using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Infrastructure;
using DDDSample1.Infrastructure.Users;
using Microsoft.AspNetCore.Identity;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Infrastructure;
using Microsoft.Extensions.Configuration;
using DDDSample1.Domain.User;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion.Internal;
using MimeKit.Cryptography;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.Utils;
using System.Linq;

namespace DDDSample1.Domain.User
{

    public class UserService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IUserRepository _repo;
        private readonly IPatientRepository _patientRepo;

        private readonly IConfiguration _configuration;

        private readonly IEmailSender _emailSender;


        public UserService(IUnitOfWork unitOfWork, IUserRepository repo, IConfiguration configuration, IEmailSender emailSender, IPatientRepository patientRepo)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
            _configuration = configuration;
            _emailSender = emailSender;
            _patientRepo = patientRepo;
        }



        public async Task<string> GetLogToken(LoginRequest request)
        {
            if (request == null)
            {
                throw new Exception("Login request is null");
            }
            Console.WriteLine("TRACK ERROR 1");

            User? user = await _repo.GetByEmailAsync(request.Email);
            if (user == null)
            {
                throw new Exception("User not found");
            }

            bool result2 = user.checkIfAccountIsBlocked();
            Console.WriteLine("TRACK ERROR 2--- resultado > " + result2);


            if (result2)
            {
                throw new Exception("Account blocked, please wait or contact the admin");
            }


            PasswordHasher passwordHasher = new();
            var result = passwordHasher.VerifyPassword(user.password.password, request.Password);

            if (!result)
            {
                int maxAllowedFailCounter = int.Parse(_configuration["Auth:MaxAllowedFailCounter"]);
                int lockoutTime = int.Parse(_configuration["Auth:LockoutTime"]);
                int failsLeft = maxAllowedFailCounter - user.loginFailCounter.loginFailCounter - 1;
                user.IncreaseFailCounter(maxAllowedFailCounter, lockoutTime);
                await this._unitOfWork.CommitAsync();

                if (failsLeft == 0)
                {
                    await this.SendEmailForAbusiveAccountAccess(user.email.email);
                    throw new Exception("Account blocked, please wait or contact the admin");

                }
                throw new Exception("Invalid password you have " + failsLeft + " attempts left");
            }


            //Falta apenas notificar o admin que a conta bloqueou

            TokenProvider tokenProvider = new TokenProvider(_configuration);
            string token = tokenProvider.Create(user);

            return token;
        }


        //Metodo do serviço para registar o user no sistema
        public async Task<User> AddAsync(User user)
        {

            bool exists = await this._repo.CheckEmail(user.email.email);

            if (exists) throw new BusinessRuleValidationException("email or phone number already exists");

            await _repo.AddAsync(user);

            await _unitOfWork.CommitAsync();

            return user;
        }


        public async Task<List<User>> GetAllAsync()
        {
            return await _repo.GetAllAsync();
        }

        public async Task<User> GetByEmailAsync(string email)
        {
            return await _repo.GetByEmailAsync(email);
        }

        public async Task<string> GenerateResetPasswordToken(User user)
        {
            TokenProvider tokenProvider = new TokenProvider(_configuration);
            string token = tokenProvider.CreatePasswordResetToken(user);

            //definir o tempo de expiração do token num config file 
            user.SetResetPasswordToken(token, DateTime.UtcNow.AddHours(24));
            await this._unitOfWork.CommitAsync();

            return token;

        }

        public async Task<string> SendEmailForAbusiveAccountAccess(string accountBlockedEmail)
        {
            await _emailSender.SendEmailAsync("Account with username " + accountBlockedEmail + " has been blocked due to abusive access / failing consequitive logins", (_configuration["Admin:Email"]), "Account Blocked");
            return "Email sent";
        }




        public async Task<string> ResetPassword(User user, string newPassword, string token)
        {

            if (!user.resetPasswordToken.resetPasswordToken.Equals(token))
            {
                throw new Exception("Invalid token");
            }

            //FALTA VERIFICAR SE O TOKEN AINDA É VALIDO EM TEMPO ?

            //DEFINIR O PASSWORD HASHER COM O AWAIT qnd tiver tempo
            PasswordHasher passwordHasher = new PasswordHasher();
            string newPasswordHash = passwordHasher.HashPassword(newPassword);
            user.SetPassword(passwordHasher.HashPassword(newPassword));

            user.ClearResetPasswordToken();

            await this._unitOfWork.CommitAsync();

            return "Success"; ;

        }


        public async Task<string> sendEmailWithUrlResetPassword(string email, string url)
        {
            await _emailSender.SendEmailAsync($"Please reset your password by clicking here: <a href='{url}'>link</a>", email, "ResetPassword");

            return "Email sent";

        }

        public async Task<ConfirmationRegisterPatientDto> RegisterPatientAsync(RegisteringPatientDto dto)
        {
            ValidatesEmailIsUnique(dto.Email);

            ValidatePassword(dto.Password);

            var patient = await _patientRepo.GetByNameEmailPhoneAsync(dto.Name, dto.Email, dto.PhoneNumber);

            if (patient == null)
            {
                throw new BusinessRuleValidationException("Patient record not found");
            }

            ValidatesPatientIsRegistered(patient);

            PasswordHasher passwordHasher = new PasswordHasher();
            string password = passwordHasher.HashPassword(dto.Password);

            var user = new User(dto.Email, "patient", password);

            await _repo.AddAsync(user);

            ConfirmationRegisterPatientDto confirmationRegisterPatientDto = await GenerateConfirmationRegisterPatientToken(user);

            SendEmailWithUrlConfirmationRegisterPatient(dto.Email, confirmationRegisterPatientDto.Token);

            return confirmationRegisterPatientDto;
        }

        public async Task<PatientDto> ConfirmRegisterPatientAsync(ConfirmationRegisterPatientDto dto)
        {
            User user = await _repo.GetByEmailAsync(dto.Email);

            if (user == null)
            {
                throw new BusinessRuleValidationException("User not found");
            }

            validatesConfirmationRegisterPatientToken(dto.Token, user);

            Patient patient = await _patientRepo.GetByEmailAsync(dto.Email);

            if (patient == null)
            {
                throw new BusinessRuleValidationException("Patient not found");
            }

            user.ConfirmAccount();

            await _unitOfWork.CommitAsync();

            return PatientMapper.ToDto(patient);
        }

        private void ValidatesEmailIsUnique(string email)
        {
            if (_repo.CheckEmail(email).Result)
            {
                throw new BusinessRuleValidationException("Email already exists");
            }
        }

        private void ValidatesPatientIsRegistered(Patient patient)
        {
            if (patient.User != null)
            {
                throw new BusinessRuleValidationException("Patient already registered");
            }
        }

        private async Task<ConfirmationRegisterPatientDto> GenerateConfirmationRegisterPatientToken(User user)
        {
            TokenProvider tokenProvider = new TokenProvider(_configuration);
            string token = tokenProvider.CreateConfirmationRegisterPatientToken(user);

            //definir o tempo de expiração do token num config file 
            user.SetConfirmationRegisterPatientToken(token, DateTime.UtcNow.AddHours(24));
            await this._unitOfWork.CommitAsync();

            return new ConfirmationRegisterPatientDto(token, user.email.email);
        }

        private void validatesConfirmationRegisterPatientToken(string token, User user)
        {
            if (user.confirmationRegisterPatientToken.Token != token)
            {
                throw new BusinessRuleValidationException("Invalid token");
            }
        }

        private async void SendEmailWithUrlConfirmationRegisterPatient(string email, string token)
        {
            string callbackUrl = $"http://localhost:9999/resetpassword?code={token}&Email={email}";
            await _emailSender.SendEmailAsync($"Please reset your password by clicking here: <a href='{callbackUrl}'>link</a>", email, "ResetPassword");
        }

        private void ValidatePassword(string password)
        {
            if (password.Length < 10)
            {
                throw new BusinessRuleValidationException("Password must have at least 10 characters");
            }
            if (!password.Any(char.IsUpper))
            {
                throw new BusinessRuleValidationException("Password must contain at least one uppercase letter");
            }

            if (!password.Any(char.IsDigit))
            {
                throw new BusinessRuleValidationException("Password must contain at least one number");
            }

            if (!password.Any(ch => !char.IsLetterOrDigit(ch)))
            {
                throw new BusinessRuleValidationException("Password must contain at least one special character");
            }
        }


    }
}