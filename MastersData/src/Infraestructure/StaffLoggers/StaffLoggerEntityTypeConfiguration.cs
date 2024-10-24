using DDDSample1.Domain.PatientLoggers;
using DDDSample1.Domain.StaffLoggers;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDSample.Infrastructure.StaffLoggers
{

    internal class StaffLoggerEntityTypeConfiguration : IEntityTypeConfiguration<StaffLogger>
    {

        public void Configure(EntityTypeBuilder<StaffLogger> builder)
        {
            builder.HasKey(b => b.Id);
        }

    }
}