

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
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;
using Microsoft.AspNetCore.Routing;
using DDDSample1.Domain.PatientLoggers;
using Microsoft.AspNetCore.Mvc;

namespace DDDSample1.Domain.User
{

    public class UserService : IUserService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IUserRepository _repo;
        private readonly IPatientRepository _patientRepo;

        private readonly IPatientLoggerRepository _patientLoggerRepository;

        private readonly IConfiguration _configuration;

        private readonly IEmailSender _emailSender;


        public UserService(IUnitOfWork unitOfWork, IUserRepository repo, IConfiguration configuration, IEmailSender emailSender, IPatientRepository patientRepo, IPatientLoggerRepository patientLoggerRepository)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
            _configuration = configuration;
            _emailSender = emailSender;
            _patientRepo = patientRepo;
            _patientLoggerRepository = patientLoggerRepository;
        }

        public async Task<string> GetLogToken(LoginRequest request)
        {
            if (request == null)
            {
                throw new Exception("Login request is null");
            }

            User? user = await _repo.GetByEmailAsync(request.Email);
            if (user == null)
            {
                throw new Exception("User not found");
            }

            bool result2 = user.checkIfAccountIsBlocked();


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

        private string GenerateResetPasswordToken(User user)
        {
            TokenProvider tokenProvider = new TokenProvider(_configuration);
            string token = tokenProvider.CreatePasswordResetToken(user);

            return token;

        }

        public async Task<string> SendEmailForAbusiveAccountAccess(string accountBlockedEmail)
        {
            await _emailSender.SendEmailAsync("Account with username " + accountBlockedEmail + " has been blocked due to abusive access / failing consequitive logins", (_configuration["Admin:Email"]), "Account Blocked");
            return "Email sent";
        }
        public async Task<string> ResetPassword(User user, string newPassword, string token)
        {
            Console.WriteLine("Ver se password sao diff");
            if (!user.resetPasswordToken.resetPasswordToken.Equals(token))
            {
                throw new Exception("Invalid token");
            }

            Console.WriteLine("Passou Ver se password sao diff");


            PasswordHasher passwordHasher = new PasswordHasher();
            user.SetPassword(passwordHasher.HashPassword(newPassword));

            Console.WriteLine("Passou SetPassword");
            user.ClearResetPasswordToken();

            Console.WriteLine("Passou ClearResetPasswordToken");
            await this._unitOfWork.CommitAsync();

            return "Success"; ;

        }


        public async Task<string> sendEmailWithUrlResetPassword(string email)
        {
            User user = await _repo.GetByEmailAsync(email);
            string token = GenerateResetPasswordToken(user);
            user.SetResetPasswordToken(token, DateTime.UtcNow.AddMinutes(5));
            
            string url = $"http://localhost:4200/reset-password?token={token}&email={email}";
            await _emailSender.SendEmailAsync($"Please reset your password by clicking here: <a href='{url}'>link</a>", email, "ResetPassword");
            await _unitOfWork.CommitAsync();


            return "Email sent";

        }

        public async Task<ConfirmationPatientDto> RegisterPatientAsync(RegisteringPatientDto dto)
        {
            ValidatesEmailIsUnique(dto.Email);

            bool passwordSatiesfied = PasswordPolicy.IsSatisfiedBy(dto.Password);

            if (!passwordSatiesfied)
            {
                throw new BusinessRuleValidationException("Password does not meet the requirements");
            }

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

            TokenProvider tokenProvider = new TokenProvider(_configuration);
            string token = tokenProvider.CreateConfirmationRegisterPatientToken(user);
            user.SetConfirmationRegisterPatientToken(token, DateTime.UtcNow.AddHours(24));

            SendEmailWithUrlConfirmationRegisterPatient(dto.Email, token);

            await _unitOfWork.CommitAsync();

            return new ConfirmationPatientDto(token, dto.Email);
        }

        public async Task<UserDTO> ConfirmRegisterPatientAsync(ConfirmationPatientDto dto)
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

            patient.AssociateUser(user);

            user.ConfirmAccount();

            await _unitOfWork.CommitAsync();

            return UserMapper.ToDto(user);
        }

        public async Task<ConfirmationEditPatientDto> EditPatientAsync(EditingPatientDto dto)
        {
            var user = await _repo.GetByEmailAsync(dto.Email);

            if (user == null)
            {
                throw new BusinessRuleValidationException("User not found");
            }

            var patient = await _patientRepo.GetByEmailAsync(dto.Email);

            if (patient == null)
            {
                throw new BusinessRuleValidationException("Patient not found");
            }

            if (!string.IsNullOrWhiteSpace(dto.EmailToEdit))
            {
                bool valid = await ValidatePatientNewEmailIsUnique(dto.EmailToEdit);
                if (!valid)
                {
                    throw new BusinessRuleValidationException("Email already exists in a patient record");
                }
            }

            if (!string.IsNullOrWhiteSpace(dto.PhoneNumberToEdit))
            {
                bool valid = await ValidatePatientNewPhoneNumberIsUnique(dto.PhoneNumberToEdit);
                if (!valid)
                {
                    throw new BusinessRuleValidationException("Phone number already exists in a patient record");
                }
            }

            LogPatientChanges(patient, "update");

            if (!string.IsNullOrWhiteSpace(dto.NameToEdit))
            {
                patient.ChangeFullName(dto.NameToEdit);
            }

            string? token = null;
            string? email = null;
            string? emailToEdit = null;
            string? phoneNumberToEdit = null;

            if (!string.IsNullOrWhiteSpace(dto.EmailToEdit) || !string.IsNullOrWhiteSpace(dto.PhoneNumberToEdit))
            {

                TokenProvider tokenProvider = new TokenProvider(_configuration);
                token = tokenProvider.CreateConfirmationRegisterPatientToken(user);

                user.SetConfirmationEditPatientToken(token, DateTime.UtcNow.AddHours(24));

                if (!string.IsNullOrWhiteSpace(dto.EmailToEdit))
                {
                    emailToEdit = dto.EmailToEdit;
                }

                if (!string.IsNullOrWhiteSpace(dto.PhoneNumberToEdit))
                {
                    phoneNumberToEdit = dto.PhoneNumberToEdit;
                }

                email = dto.Email;
                SendEmailWithUrlConfirmationEdtPatient(dto.Email, token, dto.EmailToEdit, dto.PhoneNumberToEdit);
            }

            await _unitOfWork.CommitAsync();

            return new ConfirmationEditPatientDto(token, email, emailToEdit, phoneNumberToEdit);
        }

        public async Task<UserDTO> ConfirmEditPatientAsync(ConfirmationEditPatientDto dto)
        {
            User user = await _repo.GetByEmailAsync(dto.Email);

            if (user == null)
            {
                throw new BusinessRuleValidationException("User not found");
            }

            if (user.confirmationEditPatientToken.Token != dto.Token)
            {
                throw new BusinessRuleValidationException("Invalid token");
            }

            Patient patient = await _patientRepo.GetByEmailAsync(dto.Email);

            if (patient == null)
            {
                throw new BusinessRuleValidationException("Patient not found");
            }

            if (!string.IsNullOrWhiteSpace(dto.EmailToEdit))
            {
                user.ChangeEmail(dto.EmailToEdit);
                patient.ChangeEmail(dto.EmailToEdit);
            }

            if (!string.IsNullOrWhiteSpace(dto.PhoneNumberToEdit))
            {
                patient.ChangePhoneNumber(dto.PhoneNumberToEdit);
            }

            user.ClearConfirmationEditPatientToken();

            LogPatientChanges(patient, "update");

            await _unitOfWork.CommitAsync();

            return UserMapper.ToDto(user);
        }

        public async Task<ConfirmationPatientDto> DeletePatientAsync(string email)
        {
            User user = await _repo.GetByEmailAsync(email);

            if (user == null)
            {
                throw new BusinessRuleValidationException("User not found");
            }

            TokenProvider tokenProvider = new TokenProvider(_configuration);
            string token = tokenProvider.CreateConfirmationDeletePatientToken(user);
            user.SetConfirmationDeletePatientToken(token, DateTime.UtcNow.AddHours(24));

            SendEmailWithUrlConfirmationDeletePatient(email, token);

            await _unitOfWork.CommitAsync();

            return new ConfirmationPatientDto(token, email);
        }

        public async Task ConfirmDeletePatientAsync(ConfirmationPatientDto dto)
        {
            User user = await _repo.GetByEmailAsync(dto.Email);

            if (user == null)
            {
                throw new BusinessRuleValidationException("User not found");
            }

            if (user.confirmationDeletePatientToken.Token != dto.Token)
            {
                throw new BusinessRuleValidationException("Invalid token");
            }

            Patient patient = await _patientRepo.GetByEmailAsync(dto.Email);

            if (patient == null)
            {
                throw new BusinessRuleValidationException("Patient not found");
            }

            _repo.Remove(user);
            _patientRepo.Remove(patient);

            LogPatientChanges(patient, "delete");

            await _unitOfWork.CommitAsync();
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

        private void validatesConfirmationRegisterPatientToken(string token, User user)
        {
            if (user.confirmationRegisterPatientToken.Token != token)
            {
                throw new BusinessRuleValidationException("Invalid token");
            }
        }

        private void SendEmailWithUrlConfirmationRegisterPatient(string email, string token)
        {
            string callbackUrl = $"http://localhost:4200/register/confirm?token={token}&email={email}";
            _emailSender.SendEmailAsync($"Please confirm your register here: <a href='{callbackUrl}'>link</a>", email, "Confirm the register of your account");
        }

        private void SendEmailWithUrlConfirmationEdtPatient(string email, string token, string emailToEdit, string phoneNumberToEdit)
        {
            string callbackUrl = $"http://localhost:4200/edit/confirm?token={token}&email={email}&emailToEdit={emailToEdit}&phoneNumberToEdit={phoneNumberToEdit}";
            _emailSender.SendEmailAsync($"Please confirm your edition here: <a href='{callbackUrl}'>link</a>", email, "Confirm the edition of your account");
        }

        private void SendEmailWithUrlConfirmationDeletePatient(string email, string token)
        {
            string callbackUrl = $"http://localhost:5000/api/users/patients/delete/confirmation/{token}";
            _emailSender.SendEmailAsync($"Please confirm your deletion here: <a href='{callbackUrl}'>link</a>", email, "Confirm the deletion of your account");
        }

        private async Task<bool> ValidatePatientNewEmailIsUnique(string email)
        {
            var existingPatient = await _patientRepo.GetByEmailAsync(email);
            if (existingPatient != null)
            {
                return false;
            }
            return true;
        }

        private async Task<bool> ValidatePatientNewPhoneNumberIsUnique(string phoneNumber)
        {
            var existingPatient = await _patientRepo.GetByPhoneNumberAsync(phoneNumber);
            if (existingPatient != null)
            {
                return false;
            }
            return true;
        }

        private void LogPatientChanges(Patient patient, string typeOfChange)
        {
            var patientLogger = new PatientLogger(
                patient.Id,
                patient.MedicalRecordNumber._medicalRecordNumber,
                patient.MedicalHistory.MedicalConditions?.medicalConditions ?? null,
                typeOfChange
            );

            _patientLoggerRepository.AddAsync(patientLogger);
        }

        public async Task<string> GenerateGoogleTokenFromJwt(string email, string role)
        {

            TokenProvider tokenProvider = new TokenProvider(_configuration);
            string token = tokenProvider.CreateTokenForGoogle(email, role);

            return token;
        }

        public async Task<UserDTO> GetByIdAsync(UserId id)
        {
            var user = await _repo.GetByIdAsync(id);

            return user == null ? null : UserMapper.ToDto(user);
        }

        public async Task<UserDTO> GetByEmailSearchAsync(string email)
        {
            var user = await _repo.GetByEmailAsync(email);

            return user == null ? null : UserMapper.ToDto(user);
        }
    }
}