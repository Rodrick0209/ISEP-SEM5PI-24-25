using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Threading.Tasks;
using System.Xml;
using DDDSample1.Domain.PatientLoggers;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.User;
using DDDSample1.Domain.Utils;
using DDDSample1.Infrastructure;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Storage;
using Microsoft.IdentityModel.Tokens;

namespace DDDSample1.Domain.Patients
{
    public class PatientService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IPatientRepository _patientRepository;
        private readonly IPatientLoggerRepository _patientLoggerRepository;
        private readonly IEmailSender _emailSender;

        public PatientService(IUnitOfWork unitOfWork, IPatientRepository patientRepository, IPatientLoggerRepository patientLoggerRepository, IEmailSender emailSender)
        {
            _unitOfWork = unitOfWork;
            _patientRepository = patientRepository;
            _patientLoggerRepository = patientLoggerRepository;
            _emailSender = emailSender;
        }

        public async Task<PatientDto> CreateAsync(CreatingPatientProfileDto dto)
        {

            if (string.IsNullOrWhiteSpace(dto.FirstName))
            {
                throw new BusinessRuleValidationException("Invalid first name");
            }

            if (string.IsNullOrWhiteSpace(dto.LastName))
            {
                throw new BusinessRuleValidationException("Invalid last name");
            }

            bool emailIsUnique = await ValidateEmailIsUnique(dto.Email);
            bool phoneNumberIsUnique = await ValidatePhoneNumberIsUnique(dto.PhoneNumber);
            if (!emailIsUnique || !phoneNumberIsUnique)
            {
                throw new BusinessRuleValidationException("Email and/or Phone Number are not unique");
            }

            FullName fullName = new FullName(dto.FullName);
            DateOfBirth dateOfBirth = new DateOfBirth(DateTime.Parse(dto.DateOfBirth));
            Gender gender = new Gender(dto.Gender);
            Email email = new Email(dto.Email);
            PhoneNumber phoneNumber = new PhoneNumber(dto.PhoneNumber);
            EmergencyContact emergencyContact = new EmergencyContact(dto.EmergencyContact);

            MedicalRecordNumber medicalRecordNumber = MedicalRecordNumberGenerator.GenerateMedicalRecordNumber();

            var patient = new Patient(
                fullName,
                dateOfBirth,
                gender,
                email,
                phoneNumber,
                emergencyContact,
                medicalRecordNumber
            );

            await _patientRepository.AddAsync(patient);
            await _unitOfWork.CommitAsync();

            return PatientMapper.ToDto(patient);
        }


        public async Task<PatientDto> UpdateAsync(EditingPatientProfileDto dto)
        {
            var patient = await _patientRepository.GetByMedicalRecordNumberAsync(dto.MedicalRecordNumber);

            if (patient == null)
            {
                throw new BusinessRuleValidationException("Patient not found");
            }

            string email = patient.Email.email;

            if (!string.IsNullOrWhiteSpace(dto.FullName))
            {
                FullName fullName = new FullName(dto.FullName);
                patient.ChangeFullName(fullName);
            }

            if (!string.IsNullOrWhiteSpace(dto.Email))
            {
                bool emailIsUnique = await ValidateEmailIsUnique(dto.Email);
                if (!emailIsUnique)
                {
                    throw new BusinessRuleValidationException("Email already exists");
                }
                Email newEmail = new Email(dto.Email);
                patient.ChangeEmail(newEmail);
            }

            if (!string.IsNullOrWhiteSpace(dto.PhoneNumber))
            {
                bool phoneNumberIsUnique = await ValidatePhoneNumberIsUnique(dto.PhoneNumber);
                if (!phoneNumberIsUnique)
                {
                    throw new BusinessRuleValidationException("Phone Number already exists");
                }
                PhoneNumber phoneNumber = new PhoneNumber(dto.PhoneNumber);
                patient.ChangePhoneNumber(phoneNumber);
            }


            if (!string.IsNullOrWhiteSpace(dto.MedicalConditions))
            {
                MedicalConditions medicalConditions = new MedicalConditions(dto.MedicalConditions);
                patient.ChangeMedicalConditions(medicalConditions);
            }

            LogChanges(patient, "update");

            await _unitOfWork.CommitAsync();

            if (dto.Email != null || dto.PhoneNumber != null)
            {
                await _emailSender.SendEmailAsync("Your profile has been updated. If you did not make this change, please contact support immediately.", email, "Profile Update Notification");
            }

            return PatientMapper.ToDto(patient);
        }

        public async Task DeleteAsync(DeletingPatientProfileConfirmationDto dto)
        {
            var patient = await _patientRepository.GetByMedicalRecordNumberAsync(dto.MedicalRecordNumber);

            if (patient == null)
            {
                throw new BusinessRuleValidationException("Patient not found");
            }

            _patientRepository.Remove(patient);

            LogChanges(patient, "delete");

            await _unitOfWork.CommitAsync();
        }


        public async Task<List<PatientDto>> GetAllAsync()
        {
            var list = await _patientRepository.GetAllAsync();

            List<PatientDto> listDto = list.ConvertAll<PatientDto>(pat => PatientMapper.ToDto(pat));

            return listDto;
        }

        public async Task<PatientDto> GetByMedicalRecordNumberAsync(string medicalRecordNumber)
        {
            var patient = await _patientRepository.GetByMedicalRecordNumberAsync(medicalRecordNumber);

            return patient == null ? null : PatientMapper.ToDto(patient);
        }

        public async Task<PatientDto> GetByIdAsync(PatientId id)
        {
            var patient = await _patientRepository.GetByIdAsync(id);

            return patient == null ? null : PatientMapper.ToDto(patient);
        }

        private async Task<bool> ValidateEmailIsUnique(string email)
        {
            var existingPatient = await _patientRepository.GetByEmailAsync(email);
            if (existingPatient != null)
            {
                return false;
            }
            return true;
        }

        private async Task<bool> ValidatePhoneNumberIsUnique(string phoneNumber)
        {
            var existingPatient = await _patientRepository.GetByPhoneNumberAsync(phoneNumber);
            if (existingPatient != null)
            {
                return false;
            }
            return true;
        }

        private void LogChanges(Patient patient, string typeOfChange)
        {
            var patientLogger = new PatientLogger(
                patient.Id,
                patient.FullName.fullName,
                patient.DateOfBirth.dateOfBirth.ToString("yyyy-MM-dd"),
                patient.Gender.gender,
                patient.Email.email,
                patient.PhoneNumber.phoneNumber,
                patient.EmergencyContact.emergencyContact,
                patient.MedicalRecordNumber._medicalRecordNumber,
                patient.MedicalConditions?.medicalConditions ?? null,
                typeOfChange,
                DateTime.Now
            );

            _patientLoggerRepository.AddAsync(patientLogger);
        }

        public async Task CleanupOldPatientLogs(TimeSpan retentionPeriod)
        {
            var cutoffDate = DateTime.UtcNow - retentionPeriod;

            var oldLogs = await _patientLoggerRepository.GetOldLogsAsync(cutoffDate);

            foreach (var log in oldLogs)
            {
                var cleanedLog = new PatientLogger(
                    log.MedicalRecordNumber,
                    log.FullName,
                    log.ModificationDate
                );

                _patientLoggerRepository.Remove(log);
                await _patientLoggerRepository.AddAsync(cleanedLog);
            }

            await _unitOfWork.CommitAsync();
        }


    }
}
