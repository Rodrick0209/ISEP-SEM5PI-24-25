

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

namespace DDDSample1.Domain.User
{

    public class UserService
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

        public async Task<ConfirmationPatientDto> RegisterPatientAsync(RegisteringPatientDto dto)
        {
            ValidatesEmailIsUnique(dto.Email);

            bool passwordSatiesfied = PasswordPolicy.IsSatisfiedBy(dto.Password);

            if (!passwordSatiesfied)
            {
                throw new BusinessRuleValidationException("Password does not meet the requirements");
            }

            var patient = await _patientRepo.GetByNameEmailPhoneAddressAsync(dto.Name, dto.Email, dto.PhoneNumber, dto.Street, dto.PostalCode, dto.City, dto.Country);

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

        public async Task<PatientDto> ConfirmRegisterPatientAsync(ConfirmationPatientDto dto)
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

            return PatientMapper.ToDto(patient);
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
                ValidatePatientNewEmailIsUnique(dto.EmailToEdit);
            }

            if (!string.IsNullOrWhiteSpace(dto.PhoneNumberToEdit))
            {
                ValidatePatientNewPhoneNumberIsUnique(dto.PhoneNumberToEdit);
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

                if (!string.IsNullOrWhiteSpace(emailToEdit))
                {
                    emailToEdit = dto.EmailToEdit;
                }

                if (!string.IsNullOrWhiteSpace(phoneNumberToEdit))
                {
                    phoneNumberToEdit = dto.PhoneNumberToEdit;
                }

                email = dto.Email;
                SendEmailWithUrlConfirmationEdtPatient(dto.Email, token);
            }

            await _unitOfWork.CommitAsync();

            return new ConfirmationEditPatientDto(token, email, emailToEdit, phoneNumberToEdit);
        }

        public async Task<PatientDto> ConfirmEditPatientAsync(ConfirmationEditPatientDto dto)
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
                patient.ChangeEmail(dto.EmailToEdit);
            }

            if (!string.IsNullOrWhiteSpace(dto.PhoneNumberToEdit))
            {
                patient.ChangePhoneNumber(dto.PhoneNumberToEdit);
            }

            user.ClearConfirmationEditPatientToken();

            LogPatientChanges(patient, "update");

            await _unitOfWork.CommitAsync();

            return PatientMapper.ToDto(patient);
        }

        public async Task<ConfirmationPatientDto> DeletePatientAsync(DeletingPatientDto dto)
        {
            User user = await _repo.GetByEmailAsync(dto.Email);

            if (user == null)
            {
                throw new BusinessRuleValidationException("User not found");
            }

            TokenProvider tokenProvider = new TokenProvider(_configuration);
            string token = tokenProvider.CreateConfirmationDeletePatientToken(user);
            user.SetConfirmationDeletePatientToken(token, DateTime.UtcNow.AddHours(24));

            SendEmailWithUrlConfirmationDeletePatient(dto.Email, token);

            await _unitOfWork.CommitAsync();

            return new ConfirmationPatientDto(token, dto.Email);
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
            string callbackUrl = $"http://localhost:5000/api/users/patients/confirmation/{token}";
            _emailSender.SendEmailAsync($"Please confirm your register here: <a href='{callbackUrl}'>link</a>", email, "Confirm the register og your account");
        }

        private void SendEmailWithUrlConfirmationEdtPatient(string email, string token)
        {
            string callbackUrl = $"http://localhost:5000/api/users/patients/edit/confirmation/{token}";
            _emailSender.SendEmailAsync($"Please confirm your edition here: <a href='{callbackUrl}'>link</a>", email, "Confirm the edition of your account");
        }

        private void SendEmailWithUrlConfirmationDeletePatient(string email, string token)
        {
            string callbackUrl = $"http://localhost:5000/api/users/patients/delete/confirmation/{token}";
            _emailSender.SendEmailAsync($"Please confirm your deletion here: <a href='{callbackUrl}'>link</a>", email, "Confirm the deletion of your account");
        }

        private void ValidatePatientNewEmailIsUnique(string newEmail)
        {
            var existingPatient = _patientRepo.GetByEmailAsync(newEmail);

            if (existingPatient != null)
            {
                throw new BusinessRuleValidationException("Email already used by other patient record)");
            }
        }

        private void ValidatePatientNewPhoneNumberIsUnique(string newPhoneNumber)
        {
            var existingPatient = _patientRepo.GetByPhoneNumberAsync(newPhoneNumber);

            if (existingPatient != null)
            {
                throw new BusinessRuleValidationException("Phone Number already used by other patient record");
            }
        }

        private void LogPatientChanges(Patient patient, string typeOfChange)
        {
            var patientLogger = new PatientLogger(
                patient.Id,
                patient.FullName.fullName,
                patient.DateOfBirth.dateOfBirth.ToString("yyyy-MM-dd"),
                patient.Gender.gender,
                patient.Email.email,
                patient.PhoneNumber.phoneNumber,
                patient.EmergencyContact.Name.fullName,
                patient.EmergencyContact.Email.email,
                patient.EmergencyContact.PhoneNumber.phoneNumber,
                patient.MedicalRecordNumber._medicalRecordNumber,
                patient.MedicalConditions?.medicalConditions ?? null,
                typeOfChange,
                DateTime.UtcNow
            );

            _patientLoggerRepository.AddAsync(patientLogger);
        }
    }
}