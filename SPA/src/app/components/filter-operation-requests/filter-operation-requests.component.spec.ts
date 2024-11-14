import { ComponentFixture, TestBed } from '@angular/core/testing';

import { FilterOperationRequestsComponent } from './filter-operation-requests.component';

describe('FilterOperationRequestsComponent', () => {
  let component: FilterOperationRequestsComponent;
  let fixture: ComponentFixture<FilterOperationRequestsComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [FilterOperationRequestsComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(FilterOperationRequestsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
