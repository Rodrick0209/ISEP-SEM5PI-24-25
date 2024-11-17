import { ComponentFixture, TestBed } from '@angular/core/testing';
import { StaffsComponent } from './staffs.component';
import { ListStaffsComponent } from '../list-staffs/list-staffs.component';
import { CreateStaffComponent } from '../create-staff/create-staff.component';
import { NO_ERRORS_SCHEMA } from '@angular/core';

describe('StaffsComponent', () => {
  let component: StaffsComponent;
  let fixture: ComponentFixture<StaffsComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [StaffsComponent, ListStaffsComponent, CreateStaffComponent],
      schemas: [NO_ERRORS_SCHEMA],
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(StaffsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should add a new staff member to the list', () => {
    const newStaff = 'Jane Doe';
    component.onPatientAdded(newStaff);
    expect(component.staffs).toContain(newStaff);
  });

  it('should have an empty initial staff list', () => {
    expect(component.staffs.length).toBe(0);
  });
});
