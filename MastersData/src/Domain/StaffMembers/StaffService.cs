using System.Threading.Tasks;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Utils;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Domain.Specializations;
using DDDSample1.Domain.StaffLoggers;
using System;
using DDDSample1.Domain.User;
using Org.BouncyCastle.Asn1.Misc;



namespace DDDSample1.Domain.StaffMembers
{
    public class StaffService : IStaffService
    {
        private readonly IUnitOfWork _unitOfWork;

        private readonly IStaffRepository _staffRepository;
        private readonly IAvailabilitySlotsRepository _availabilitySlotsRepository;

        private readonly ISpecializationRepository _specializationRepository;
        private readonly IEmailSender _emailSender;
        private readonly IStaffLoggerRepository _staffLoggerRepository;






        public StaffService(IUnitOfWork unitOfWork, IStaffRepository staffRepository, IAvailabilitySlotsRepository availabilitySlotsRepository, ISpecializationRepository specializationRepository, IEmailSender emailSender, IStaffLoggerRepository staffLoggerRepository)
        {
            _unitOfWork = unitOfWork;
            _staffRepository = staffRepository;
            _availabilitySlotsRepository = availabilitySlotsRepository;
            _specializationRepository = specializationRepository;
            _emailSender = emailSender;
            _staffLoggerRepository = staffLoggerRepository;

        }


        public async Task<StaffDto> AddAsync(StaffDto staffdto)
        {


            bool emailIsUnique = await validateEmailIsUnique(staffdto.Email);
            bool phoneNumberIsUnique = await validatePhoneNumberIsUnique(staffdto.PhoneNumber);
            if (!emailIsUnique || !phoneNumberIsUnique)
            {
                throw new BusinessRuleValidationException("Email and/or Phone Number are not unique");
            }

            await checkOSpecializationIdAsync(staffdto.SpecializationId);

            
            //await checkAvailabilitySlotIdAsync(staffdto.AvailabilitySlotsId);

            DateTime recruitmentDate = DateTime.Now;

            StaffIdGeneratorService staffIdGeneratorService = new StaffIdGeneratorService();
            Category category = Enum.Parse<Category>(staffdto.Category);
            StaffId staffId = staffIdGeneratorService.generateStaffId(category, recruitmentDate);
            Staff staff = StaffMapper.toDomain(staffdto, staffId);

            await _staffRepository.AddAsync(staff);
            await _unitOfWork.CommitAsync();

            return StaffMapper.toDTO(staff);
        }

        public async Task<StaffDto> UpdateAsync(EditingStaffProfileDto dto)
        {
            var staff = await _staffRepository.GetByIdAsync(dto.Id);

            if (staff == null)
            {
                throw new BusinessRuleValidationException("Staff member not found");
            }

            var objetoLogger = LogObjectCreate(staff, LoggerTypeOfChange.Update);

            bool hasChanges = false;

            string email = staff.Email.email;

            if (!string.IsNullOrWhiteSpace(dto.FullName) && staff.FullName.fullName.Equals(dto.FullName))
            {
                staff.ChangeFullName(dto.FullName);
                hasChanges = true;
            }

            if (!string.IsNullOrWhiteSpace(dto.Email) && staff.Email.email.Equals(dto.Email))
            {
                bool emailIsUnique = await validateEmailIsUnique(dto.Email);
                if (!emailIsUnique)
                {
                    throw new BusinessRuleValidationException("Email already exists");
                }
                staff.ChangeEmail(dto.Email);
                hasChanges = true;
            }

            if (!string.IsNullOrWhiteSpace(dto.PhoneNumber) && staff.PhoneNumber.phoneNumber.Equals(dto.PhoneNumber))
            {
                bool phoneNumberIsUnique = await validatePhoneNumberIsUnique(dto.PhoneNumber);
                if (!phoneNumberIsUnique)
                {
                    throw new BusinessRuleValidationException("Phone Number already exists");
                }
                staff.ChangePhoneNumber(dto.PhoneNumber);
                hasChanges = true;
            }


            if (!string.IsNullOrWhiteSpace(dto.LicenseNumber) && staff.LicenseNumber.licenseNumber.Equals(dto.LicenseNumber))
            {
                staff.ChangeLicenseNumber(dto.LicenseNumber);
                hasChanges = true;
            }


            if (hasChanges)
            {
                await _staffLoggerRepository.AddAsync(objetoLogger);
            }

            await _unitOfWork.CommitAsync();


            if (dto.Email != null || dto.PhoneNumber != null)
            {
                await _emailSender.SendEmailAsync("Your profile has been updated. If you are not aware of this this change, please contact support immediately.", email, "Profile Update Notification");
            }

            return StaffMapper.toDTO(staff);



        }

        public async Task<Staff> DeleteAsync(StaffId id)
        {
            var staff = await _staffRepository.GetByIdAsync(id);

            if (staff == null)
            {
                throw new BusinessRuleValidationException("Staff member not found");
            }

            //var objetoLogger = LogObjectCreate(staff, LoggerTypeOfChange.Delete);
            //await _staffLoggerRepository.AddAsync(objetoLogger);

            staff.Deactivate();

            await _unitOfWork.CommitAsync();


            return staff;
        }


        public async Task<Staff> GetByIdAsync(StaffId id)
        {

            var op = await this._staffRepository.GetByIdAsync(id);
            if (op == null)
                return null;

            return op;

        }



        public async Task<Specialization> checkOSpecializationIdAsync(string specializationId)
        {

            try
            {
                var id = new SpecializationId(specializationId);
                Console.WriteLine("ID ->: " + id);
                var spec = await this._specializationRepository.GetByIdAsync(id);

                if (spec == null)
                {
                    throw new BusinessRuleValidationException("Specialization does not exist");
                }

                return spec;
            }
            catch (Exception e)
            {
                throw new BusinessRuleValidationException("Specialization does not exist");
            }
        }

        public async Task<AvailabilitySlot> checkAvailabilitySlotIdAsync(string availabilitySlotsId)
        {

            try
            {
                var id = new AvailabilitySlotsId(availabilitySlotsId);
                Console.WriteLine("ID ->: " + id);
                var slot = await this._availabilitySlotsRepository.GetByIdAsync(id);

                if (slot == null)
                {
                    throw new BusinessRuleValidationException("Availability Slot does not exist");
                }

                return slot;
            }
            catch (Exception e)
            {
                throw new BusinessRuleValidationException("Availability Slot does not exist");
            }
        }


        private async Task<bool> validateEmailIsUnique(string email)
        {
            var existingStaff = await _staffRepository.GetByEmailAsync(email);
            if (existingStaff != null)
            {
                return false;
            }
            return true;
        }

        private async Task<bool> validatePhoneNumberIsUnique(string phoneNumber)
        {
            var existingStaff = await _staffRepository.GetByPhoneNumberAsync(phoneNumber);
            if (existingStaff != null)
            {
                return false;
            }
            return true;
        }



        public async Task<List<Staff>> GetAllAsync()
        {
            return await this._staffRepository.GetAllAsync();
        }

        private StaffLogger LogObjectCreate(Staff staff, LoggerTypeOfChange typeOfChange)
        {
            return new StaffLogger(
                     staff.Id,
                     staff.FullName,
                     staff.SpecializationId,
                     staff.AvailabilitySlotsId,
                     staff.Email.email,
                     staff.PhoneNumber.phoneNumber,
                     staff.Category.ToString(),
                     typeOfChange.ToString(),
                     DateTime.UtcNow);
        }

    }
}