using DDDSample1.Domain.OperationRequestLoggers;
using DDDSample1.Domain.Shared;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;


namespace DDDSample1.Infrastructure.OperationRequestLoggers
{
    internal class OperationRequestLoggerEntityTypeConfiguration : IEntityTypeConfiguration<OperationRequestLogger>
    {

        public void Configure(EntityTypeBuilder<OperationRequestLogger> builder)
        {
            builder.HasKey(b => b.Id);
        }

    }
}