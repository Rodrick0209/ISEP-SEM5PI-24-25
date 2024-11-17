import { ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { FilterPatientsComponent } from './filter-patients.component';

describe('FilterPatientsComponent', () => {
  let component: FilterPatientsComponent;
  let fixture: ComponentFixture<FilterPatientsComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [FilterPatientsComponent, FormsModule, CommonModule],
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(FilterPatientsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should emit filterChanged event with correct filter values', () => {
    spyOn(component.filterChanged, 'emit');

    component.filter = {
      medicalRecordNumber: '123',
      name: 'John Doe',
      email: 'john.doe@example.com',
      dateOfBirth: '1990-01-01'
    };

    component.applyFilter();

    expect(component.filterChanged.emit).toHaveBeenCalledWith({
      medicalRecordNumber: '123',
      name: 'John Doe',
      email: 'john.doe@example.com',
      dateOfBirth: new Date('1990-01-01').toISOString()
    });
  });

  it('should call applyFilter on search event', () => {
    spyOn(component, 'applyFilter');

    const event = new Event('submit');
    component.onSearch(event);

    expect(component.applyFilter).toHaveBeenCalled();
  });
});